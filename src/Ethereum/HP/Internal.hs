{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Ethereum.HP.Internal
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- This is an internal module that exports the constructors for 'HexDigit' and
-- 'Nibbles', which allow to create invalid valid. Please use the module
-- "Ethereum.HP" instead.
--
-- Appendix C. Hex-Prefix Encoding
--
-- Hex-prefix encoding is an efficient method of encoding an arbitrary number of
-- nibbles as a byte array. It is able to store an additional flag which, when
-- used in the context of the trie (the only context in which it is used),
-- disambiguates between node types.
--
module Ethereum.HP.Internal
(
-- * Hex Digits
  HexDigit(..)
, upper
, lower
, hexDigits

-- * Nibbles
, Nibbles(..)
, nibbles
, toNibbles
, nnull
, nlength
, nix
, nhead
, nmaybeHead
, nsplitAt
, ndrop
, ntake
, nrange
, ntoList

-- ** Internal Tools
, nalign
, checkNibbles
, nix_
, nhead_

-- * Hex Prefix Encoding
, FlaggedNibbles(..)
, HpException(..)
, toHp
, fromHp
) where

import Control.Monad
import Control.Monad.Catch

import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import Data.Function
import qualified Data.Text as T
import Data.Word

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import GHC.Stack

import System.IO.Unsafe

import Text.Printf

-- internal modules

import Ethereum.RLP


-- -------------------------------------------------------------------------- --
-- Hex Digits

-- | Stores a hex digit in the lower four bits of a 'Word8'
--
newtype HexDigit = HexDigit { getHexDigit :: Word8 }
    deriving (Eq, Ord)

instance Show HexDigit where
    show (HexDigit x) = printf "0x%.2x" x

-- | Returns the upper bits of a 'Word8'
--
upper :: Word8 -> HexDigit
upper w = HexDigit $ w `quot` 16
{-# INLINE upper #-}

lower :: Word8 -> HexDigit
lower w = HexDigit $ w `rem` 16
{-# INLINE lower #-}

hexDigits :: Word8 -> (HexDigit, HexDigit)
hexDigits w = bimap HexDigit HexDigit $ w `quotRem` 16
{-# INLINE hexDigits #-}

-- test cases

-- -------------------------------------------------------------------------- --
-- Nibbles

-- | Key Nibbles. The internal format is optimized for avoiding copying the
-- underlying data and for sharing the data. In case nibbles should be stored
-- for longer times it is recommended to use nalign to transform the data into a
-- compact representation.
--
data Nibbles = Nibbles
    {-# UNPACK #-} !Int
        -- ^ offset (zero based index of first nibble)
        --
        -- prop> let (Nibbles o _ _) in 0 <= o
        -- prop> let (Nibbles o l d) in o + l <= 2 * length d
        --
    {-# UNPACK #-} !Int
        -- ^ length
        --
        -- prop> let (Nibbles _ l _) in 0 <= l
        -- prop> let (Nibbles o l d) in o + l <= 2 * length d
        --
    {-# UNPACK #-} !B.ByteString
        -- ^ data buffer

instance Show Nibbles where
    show (Nibbles o l d) = "Nibbles " <> show o <> " " <> show l <> " 0x"
        <> B8.unpack (B16.encode d)

instance Eq Nibbles where
    a@(Nibbles o0 l0 d0) == b@(Nibbles o1 l1 d1) = nlength a == nlength b &&
        ( (o0 == o1 && l0 == l1 && d0 == d1)
        || (l'0 == l'1 && d'0 == d'1)
        )
      where
        Nibbles _ l'0 d'0 = nalign a
        Nibbles _ l'1 d'1 = nalign b
    {-# INLINE (==) #-}

instance Ord Nibbles where
    compare = compare `on` ntoList
    {-# INLINE compare #-}

nibbles :: B.ByteString -> Int -> Int -> Nibbles
nibbles d o l = checkNibbles $ Nibbles o l d
{-# INLINE nibbles #-}

toNibbles :: B.ByteString -> Nibbles
toNibbles b = nibbles b 0 (B.length b * 2)
{-# INLINE toNibbles #-}

-- | Copy the underlying data and yield a compact representation. This
-- terminates sharing of the underlying bytes.
--
-- \(O(n)\)
--
nalign :: Nibbles -> Nibbles
nalign (Nibbles o l d)
    | even o = Nibbles 0 l $
        let b = B.take targetLength $ B.drop (o `quot` 2) d
        in if even l
          then b
          else case B.unsnoc b of
                Nothing -> b
                Just (x,y) -> B.snoc x (y .&. 0xf0)
    | otherwise = unsafePerformIO $ B.unsafeUseAsCStringLen d $ \(ptr, _) -> do
        targetBasePtr <- mallocBytes targetLength
        let sourceBasePtr = plusPtr ptr (o `quot` 2)
        forM_ [0 .. targetLength - 1] $ \x -> do
            let sourcePtr = plusPtr sourceBasePtr x
            let targetPtr = plusPtr targetBasePtr x
            let nextSourcePtr = plusPtr sourcePtr 1
            w0 <- peek @Word8 sourcePtr
            w1 <- if (x <= (l `div` 2) - 1) -- take care of the last nibble when it's undefined
              then peek @Word8 nextSourcePtr
              else return 0x00
            poke targetPtr (shiftL w0 4 .|. shiftR w1 4)
        !bytes <- B.unsafePackCStringLen (targetBasePtr, targetLength)
        return $ Nibbles 0 l bytes
  where
    targetLength = ceiling (fromIntegral @_ @Double l / 2)

instance Semigroup Nibbles where
    a <> b
        | nnull a = b
        | nnull b = a
        | even l0 =
            let Nibbles _ l1 d1 = nalign b
            in Nibbles 0 (l0 + l1) (d0 <> d1)
        | otherwise =
            let d0' = B.init d0
                (Nibbles _ l1 d1) = nalign $ ndrop 1 b
                x = (B.last d0 .&. 0xf0) .|. shiftR (getHexDigit $ nhead b) 4
            in Nibbles 0 (l0 + l1 + 1) (d0' <> B.singleton x <> d1)
      where
        Nibbles _ l0 d0 = nalign a
    {-# INLINE (<>) #-}

instance Monoid Nibbles where
    mempty = Nibbles 0 0 mempty
    {-# INLINE mempty #-}

-- | Check that a 'Nibbles' value is valid. Throws an error if the value is not
-- valid.
--
-- \(O(1)\)
--
checkNibbles :: HasCallStack => Nibbles -> Nibbles
checkNibbles n@(Nibbles o l d)
    | 0 <= o && 0 <= l && o + l <= 2 * B.length d = n
    | otherwise = error $ "invalid Nibbles: " <> show n
{-# INLINE checkNibbles #-}

nnull :: Nibbles -> Bool
nnull (Nibbles _ l _) = l == 0
{-# INLINE nnull #-}

-- |
--
-- \(O(1)\)
--
nlength :: Nibbles -> Int
nlength (Nibbles _ l _) = l
{-# INLINE nlength #-}

-- | Unsafe zero-based index for Nibbles
--
-- \(O(1)\)
--
nix :: HasCallStack => Nibbles -> Int -> HexDigit
nix ns@(Nibbles _ l _) i
    | i < 0 = error $ "Nibble index too small: " <> show i
    | i >= l = error $ "Nibble index " <> show i <> " out of range for " <> show ns
    | otherwise = nix_ ns i
{-# INLINE nix #-}

-- | Unchecked zero-based index for Nibbles.
--
-- \(O(1)\)
--
nix_ :: HasCallStack => Nibbles -> Int -> HexDigit
nix_ (Nibbles o _ bs) i
    | even (o + i) = upper $ B.index bs ((o+i) `quot` 2)
    | otherwise = lower $ B.index bs ((o+i) `quot` 2)
{-# INLINE nix_ #-}

-- | head
--
-- \(O(1)\)
--
nmaybeHead :: HasCallStack => Nibbles -> Maybe HexDigit
nmaybeHead ns
    | nnull ns = Nothing
    | otherwise = Just $ nix ns 0
{-# INLINE nmaybeHead #-}

-- | head
--
-- \(O(1)\)
--
nhead :: HasCallStack => Nibbles -> HexDigit
nhead ns = nix ns 0
{-# INLINE nhead #-}

-- | Unchecked head
--
-- \(O(1)\)
--
nhead_ :: Nibbles -> HexDigit
nhead_ ns = nix_ ns 0
{-# INLINE nhead_ #-}

-- | Like @splitAt@ for lists this is safe. If @i@ is out of bounds it returns
-- empty Nibbles.
--
-- \(O(1)\)
--
nsplitAt :: Int -> Nibbles -> (Nibbles, Nibbles)
nsplitAt i (Nibbles o l d) =
    ( Nibbles o i' d
    , Nibbles (o + i') (l - i') d
    )
  where
    i' = min (max 0 i) l
{-# INLINE nsplitAt #-}

-- |
--
-- \(O(1)\)
--
ntake :: Int -> Nibbles -> Nibbles
ntake i = fst . nsplitAt i
{-# INLINE ntake #-}

-- |
--
-- \(O(1)\)
--
ndrop :: Int -> Nibbles -> Nibbles
ndrop i = snd . nsplitAt i
{-# INLINE ndrop #-}

-- | This is safe. If the parameters are out of bound, the result is truncated
-- and possibly empty.
--
-- \(O(1)\)
--
nrange :: Int -> Int -> Nibbles -> Nibbles
nrange s u (Nibbles o l d)
    | u < s = mempty
    | otherwise = Nibbles o' l' d
  where
    o' = o + min (max 0 s) l
    l' = min (max 0 (u - s)) (max 0 (l - u))
{-# INLINE nrange #-}

ntoList :: Nibbles -> [HexDigit]
ntoList ns = fmap (nix_ ns) [0 .. nlength ns -1]
{-# INLINE ntoList #-}

-- -------------------------------------------------------------------------- --
-- Hex-Prefix Encoding

data FlaggedNibbles = FlaggedNibbles {-# UNPACK #-} !Bool {-# UNPACK #-} !Nibbles
    deriving (Show, Eq)

newtype HpException = HpException T.Text
    deriving (Show, Eq)

instance Exception HpException

-- hp :: [HexDigit] -> Bool -> B.ByteString
-- hp x t
--     | even (length x) = B.cons (16 * f t) $ B.pack bs
--     | otherwise = B.cons (16 * (f t + 1) + head bs) $ B.pack (tail bs)
--   where
--     bs = getHexDigit <$> x
--     f False = 0
--     f True = 2

toHp :: FlaggedNibbles -> B.ByteString
toHp (FlaggedNibbles t ns)
    | even (nlength ns) = B.cons (16 * f t)
        $ let Nibbles _ _ d = nalign ns in d
    | otherwise = B.cons (16 * (f t + 1) +  getHexDigit (nhead ns))
        $ let Nibbles _ _ d = nalign (ndrop 1 ns) in d
  where
    f False = 0
    f True = 2

fromHp :: MonadThrow m => B.ByteString -> m FlaggedNibbles
fromHp b = case B.uncons b of
    Nothing -> throwM $ HpException "Hex-Prefix encoding must not be of length zero"
    Just (h, t)
        | h .&. 0x10 == 0x0 -> return $ FlaggedNibbles flag (nibbles t 0 (B.length t * 2))
        | otherwise -> return $ FlaggedNibbles flag (nibbles b 1 (2 * B.length b - 1))
      where
        flag = (h .&. 0x20) == 0x20

instance RLP FlaggedNibbles where
    putRlp = putRlp . toHp
    getRlp = do
        b <- getRlp
        case fromHp b of
            Left e -> fail $ show e
            Right x -> return x

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}
