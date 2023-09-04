{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Ethereum.RLP
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- RLP -  Recursive Length Prefix Encoding
--
module Ethereum.RLP
(
-- * Put Monad
  Put
, byteCount
, builder
, putByteString
, putLazyByteString

-- ** Encoding Primitives
, put
, putShort
, put8
, put16Be
, put16Le
, put32Be
, put32Le
, put64Be
, put64Le

, putWordNBe
, putWordNLe
, putWord128Be
, putWord128Le
, putWord256Be
, putWord256Le
, putWord512Be
, putWord512Le

, putIntNBe
, putIntNLe
, putInt128Be
, putInt128Le
, putInt256Be
, putInt256Le
, putInt512Be
, putInt512Le

-- * Get Monad
, Get
, label
, get
, getLazy

-- * RLP

, RLP(..)
, putRlpLazyByteString
, putRlpByteString

-- ** Encoding Primitives
, putRlpB
, putRlpL

-- ** Decoding Primitives
, isB
, isL
, end
, getRlpB
, getRlpBSize
, getRlpL
, getRlpLSize

-- * Serialization of Generic Binary Trees
, Tree(..)

) where

import Control.Applicative
import Control.Monad
#if ! MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

import qualified Data.Binary.Get as BI
import Data.Binary.Get hiding (Get, label)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Word

import Numeric.Natural

import GHC.TypeNats

import Unsafe.Coerce

-- internal modules

import Ethereum.Utils

import Numeric.Checked
import qualified Numeric.Checked.Int as Checked
import qualified Numeric.Checked.Word as Checked

-- -------------------------------------------------------------------------- --
-- Put - A Builder that keeps track of length

-- | A Builder that keeps track of the length of the output.
--
data Put = Put {-# UNPACK #-} !Int !BB.Builder

-- | Number of bytes produced by the builder
--
byteCount :: Put -> Natural
byteCount (Put n _) = int n
{-# INLINE byteCount #-}

-- | Extract the bytestring builder from the 'Put' value.
--
builder :: Put -> BB.Builder
builder (Put _ b) = b
{-# INLINE builder #-}

instance Semigroup Put where
    Put c0 b0 <> Put c1 b1 = Put (c0 + c1) (b0 <> b1)
    {-# INLINE (<>) #-}

instance Monoid Put where
    mempty = Put 0 mempty
    {-# INLINE mempty #-}

-- | Put a single byte
--
put8 :: Word8 -> Put
put8 b = Put 1 (BB.word8 b)
{-# INLINE put8 #-}

-- | Put a 'Word16' in big endian encoding
--
put16Be :: Word16 -> Put
put16Be b = Put 2 (BB.word16BE b)
{-# INLINE put16Be #-}

-- | Put a 'Word16' in little endian encoding
--
put16Le :: Word16 -> Put
put16Le b = Put 2 (BB.word16LE b)
{-# INLINE put16Le #-}

-- | Put a 'Word32' in big endian encoding
--
put32Be :: Word32 -> Put
put32Be b = Put 4 (BB.word32BE b)
{-# INLINE put32Be #-}

-- | Put a 'Word32' in little endian encoding
--
put32Le :: Word32 -> Put
put32Le b = Put 4 (BB.word32LE b)
{-# INLINE put32Le #-}

-- | Put a 'Word64' in big endian encoding
--
put64Be :: Word64 -> Put
put64Be b = Put 8 (BB.word64BE b)
{-# INLINE put64Be #-}

-- | Put a 'Word64' in little endian encoding
--
put64Le :: Word64 -> Put
put64Le b = Put 8 (BB.word64LE b)
{-# INLINE put64Le #-}

-- | Put a 'B.ByteString'
--
put :: B.ByteString -> Put
put b = Put (B.length b) (BB.byteString b)
{-# INLINE put #-}

-- | Put a 'BS.ShortByteString'
--
putShort :: BS.ShortByteString -> Put
putShort b = Put (BS.length b) (BB.shortByteString b)
{-# INLINE putShort #-}

putWordNBe
    :: forall n
    . KnownNat n
    => KnownNat ((2 ^ n) - 1)
    => Checked.WordN n
    -> Put
putWordNBe b = Put l $ encodeNaturalBe l $ int b
  where
    l = int $ natVal_ @n

putWordNLe
    :: forall n
    . KnownNat n
    => KnownNat ((2 ^ n) - 1)
    => Checked.WordN n
    -> Put
putWordNLe b = Put l $ encodeNaturalLe l $ int b
  where
    l = int $ natVal_ @n

putWord128Be :: Checked.Word128 -> Put
putWord128Be = putWordNBe

putWord256Be :: Checked.Word256 -> Put
putWord256Be = putWordNBe

putWord512Be :: Checked.Word512 -> Put
putWord512Be = putWordNBe

putWord128Le :: Checked.Word128 -> Put
putWord128Le = putWordNBe

putWord256Le :: Checked.Word256 -> Put
putWord256Le = putWordNBe

putWord512Le :: Checked.Word512 -> Put
putWord512Le = putWordNBe

putIntNBe
    :: forall n
    . KnownNat n
    => KnownNat (2^(n-1))
    => KnownNat (2 ^(n-1)-1)
    => Checked.IntN n
    -> Put
putIntNBe b = Put l $ encodeNaturalBe l $ int b
  where
    l = int $ natVal_ @n

putIntNLe
    :: forall n
    . KnownNat n
    => KnownNat (2^(n-1))
    => KnownNat (2^(n-1)-1)
    => Checked.IntN n
    -> Put
putIntNLe b = Put l $ encodeNaturalLe l $ int b
  where
    l = int $ natVal_ @n

putInt128Be :: Checked.Int128 -> Put
putInt128Be = putIntNBe

putInt256Be :: Checked.Int256 -> Put
putInt256Be = putIntNBe

putInt512Be :: Checked.Int512 -> Put
putInt512Be = putIntNBe

putInt128Le :: Checked.Int128 -> Put
putInt128Le = putIntNBe

putInt256Le :: Checked.Int256 -> Put
putInt256Le = putIntNBe

putInt512Le :: Checked.Int512 -> Put
putInt512Le = putIntNBe

-- -------------------------------------------------------------------------- --
-- Get Monad

-- | A restricted wrapper around the 'Get' monad from the binary package along
-- with primitives for parsing RLP encoded data.
--
newtype Get a = Get (BI.Get a)
    deriving (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus)

-- | Label a 'Get' decoder. The label is used in failure messages.
--
label :: String -> Get a -> Get a
label l (Get f) = Get (BI.label l f)
{-# INLINE label #-}

-- | Check whether end of input has been reached.
--
end :: Get Bool
end = Get isEmpty
{-# INLINE end #-}

-- | Decode a value from a lazy 'BL.ByteString'.
--
getLazy :: Get a -> BL.ByteString -> Either String a
getLazy (Get g) b = case runGetOrFail (g <* isEmpty) b of
    Left (_ , _, e) -> Left e
    Right ("", _, x) -> Right x

    -- should not happen
    Right (p, n, _) -> Left $ "getLazy: "
        <> show (BL.length p) <> " pending input bytes after reading "
        <> show n <> "bytes"
{-# INLINEABLE getLazy #-}

-- runGetOrFail :: Get a -> ByteString -> Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, a)

-- | Decode a value from a 'B.ByteString'.
--
get :: Get a -> B.ByteString -> Either String a
get g = getLazy g . BL.fromStrict
{-# INLINE get #-}

-- -------------------------------------------------------------------------- --
-- BE - Serialization of unsigned integral numbers
--
-- This sections implements the BE encoding that is used in RLP. The
-- implementation is internal since BE encoded values are used only as part of
-- RLP. In particular, scalar value are first encoded to byte arrays using BE
-- and than encoded as RLP byte arrays.

-- | BE is the function that expands a non-negative integer value to a
-- big-endian byte array of minimal length and the dot operator performs
-- sequence concatenation.
--
-- There is no specific canonical encoding format for signed or floating-point
-- values.
--
class Integral a => BE a where
    putBe :: a -> Put

instance BE Word8 where
    putBe 0 = mempty
    putBe x = put8 x
    {-# INLINE putBe #-}

instance BE Word16 where
    putBe x
        | x < pow256 1 = putBe @Word8 (int x)
        | otherwise = put16Be x
    {-# INLINEABLE putBe #-}

instance BE Word32 where
    putBe x
        | x < pow256 2 = putBe @Word16 (int x)
        | x < pow256 3 = put16Be (int $ shiftR x 8) <> put8 (int x)
        | otherwise = put32Be x
    {-# INLINEABLE putBe #-}

instance BE Word64 where
    putBe x
        | x < pow256 4 = putBe @Word32 (int x)
        | x < pow256 5 = put32Be (int $ shiftR x 8) <> put8 (int x)
        | x < pow256 6 = put32Be (int $ shiftR x 16) <> put16Be (int x)
        | x < pow256 7 = put32Be (int $ shiftR x 24) <> put16Be (int $ shiftR x 8) <> put8 (int x)
        | otherwise = put64Be x
    {-# INLINEABLE putBe #-}

instance BE Natural where
    putBe x
        | x < pow256 8 = putBe @Word64 (int x)
        | otherwise = mconcat $ go x []
      where
        go n l = case quotRem n (pow256 8) of
            (0, !r) -> putBe r : l
            (!a, !r) -> go a (put64Be (int r) : l)
    {-# INLINEABLE putBe #-}

-- | Decode BE encoded fixed size words as 'Word64'
--
-- For smaller word sizes the result must be casted. There is no performance
-- benefit of decoding to smaller sizes directly.
--
getBe64 :: Word8 -> Get Word64
getBe64 = label "getBe64" . go
  where
    go 0 = return 0
    go 1 = int <$> Get getWord8
    go 2 = int <$> Get getWord16be
    go 3 = do
        !a <- flip shiftL 8 . int <$> Get getWord16be
        !b <- int <$> Get getWord8
        return $! a + b
    go 4 = int <$> Get getWord32be
    go 5 = do
        !a <- flip shiftL 8 . int <$> Get getWord32be
        !b <- int <$> Get getWord8
        return $! a + b
    go 6 = do
        !a <- flip shiftL 16 . int <$> Get getWord32be
        !b <- int <$> Get getWord16be
        return $! a + b
    go 7 = do
        !a <- flip shiftL 24 . int <$> Get getWord32be
        !b <- flip shiftL 8 . int <$> Get getWord16be
        !c <- int <$> Get getWord8
        return $! a + b + c
    go 8 = int <$> Get getWord64be
    go n = fail $ "attempt to decode more 8 bytes as Word64: " <> show n

-- | Decode an arbitrary size BE encoding scalar value.
--
getBe :: Int -> Get Natural
getBe = label "getBe" . go
  where
    go 0 = return 0
    go n
        | n < 0 = return 0
        | otherwise = (+)
            <$> ((* pow256 s) . int <$> getBe64 (int x))
            <*> go (n - x)
      where
        s = max 0 (n - 8)
        x = min 8 n
{-# INLINEABLE getBe #-}

-- -------------------------------------------------------------------------- --
-- RLP -  Recursive Length Prefix Encoding

-- | RLP -  Recursive Length Prefix Encoding
--
-- This is a serialisation method for encoding arbitrarily structured binary
-- data (byte arrays).
--
-- We define the RLP function as RLP through two sub-functions, the first
-- handling the instance when the value is a byte array (see 'putRlpB' and
-- 'getRlpB'), the second when it is a sequence of further values (see 'putRlpL'
-- and 'getRlpL').
--
class RLP a where
    putRlp :: a -> Put
    getRlp :: Get a

instance RLP B.ByteString where
    putRlp = putRlpB
    getRlp = label "ByteString" getRlpB
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance RLP BS.ShortByteString where
    putRlp = putRlpBShort
    getRlp = label "ShortByteString" $ BS.toShort <$> getRlpB
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance RLP a => RLP [a] where
    putRlp = putRlpL . fmap putRlp
    getRlp = label "[]" $ getRlpL getList
      where
        getList = end >>= \case
            True -> return []
            False -> (:) <$> getRlp <*> getList
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}
    {-# SPECIALIZE instance RLP [B.ByteString] #-}
    {-# SPECIALIZE instance RLP [BS.ShortByteString] #-}
    {-# SPECIALIZE instance RLP [Word8] #-}
    {-# SPECIALIZE instance RLP [Word16] #-}
    {-# SPECIALIZE instance RLP [Word32] #-}
    {-# SPECIALIZE instance RLP [Word64] #-}
    {-# SPECIALIZE instance RLP [Natural] #-}

instance RLP () where
    putRlp () = putRlpL []
    getRlp = label "()" $ getRlpL (() <$ Get isEmpty)
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance (RLP a0, RLP a1) => RLP (a0, a1) where
    putRlp (a0, a1) = putRlpL [putRlp a0, putRlp a1]
    getRlp = label "(,)" $ getRlpL $ (,) <$> getRlp <*> getRlp
    {-# INLINEABLE putRlp #-}
    {-# INLINEABLE getRlp #-}

instance (RLP a0, RLP a1, RLP a2) => RLP (a0, a1, a2) where
    putRlp (a0, a1, a2) = putRlpL [putRlp a0, putRlp a1, putRlp a2]
    getRlp = label "(,,)" $ getRlpL $ (,,) <$> getRlp <*> getRlp <*> getRlp
    {-# INLINEABLE putRlp #-}
    {-# INLINEABLE getRlp #-}

instance (RLP a0, RLP a1, RLP a2, RLP a3) => RLP (a0, a1, a2, a3) where
    putRlp (a0, a1, a2, a3) = putRlpL [putRlp a0, putRlp a1, putRlp a2, putRlp a3]
    getRlp = label "(,,,)" $ getRlpL $ (,,,) <$> getRlp <*> getRlp <*> getRlp <*> getRlp
    {-# INLINEABLE putRlp #-}
    {-# INLINEABLE getRlp #-}

instance (RLP a0, RLP a1, RLP a2, RLP a3, RLP a4) => RLP (a0, a1, a2, a3, a4) where
    putRlp (a0, a1, a2, a3, a4) = putRlpL [putRlp a0, putRlp a1, putRlp a2, putRlp a3, putRlp a4]
    getRlp = label "(,,,,)" $ getRlpL $ (,,,,) <$> getRlp <*> getRlp <*> getRlp <*> getRlp <*> getRlp

instance (RLP a0, RLP a1, RLP a2, RLP a3, RLP a4, RLP a5) => RLP (a0, a1, a2, a3, a4, a5) where
    putRlp (a0, a1, a2, a3, a4, a5) = putRlpL [putRlp a0, putRlp a1, putRlp a2, putRlp a3, putRlp a4, putRlp a5]
    getRlp = label "(,,,,,)" $ getRlpL $ (,,,,,) <$> getRlp <*> getRlp <*> getRlp <*> getRlp <*> getRlp <*> getRlp

-- | 17-tuple
--
instance (RLP a0, RLP a1, RLP a2, RLP a3, RLP a4, RLP a5, RLP a6, RLP a7, RLP a8, RLP a9, RLP a10, RLP a11, RLP a12, RLP a13, RLP a14, RLP a15, RLP a16) => RLP (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) where
    putRlp (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = putRlpL
        [ putRlp a0
        , putRlp a1
        , putRlp a2
        , putRlp a3
        , putRlp a4
        , putRlp a5
        , putRlp a6
        , putRlp a7
        , putRlp a8
        , putRlp a9
        , putRlp a10
        , putRlp a11
        , putRlp a12
        , putRlp a13
        , putRlp a14
        , putRlp a15
        , putRlp a16
        ]
    getRlp = label "(,,,,,,,,,,,,,,,,)" $ getRlpL $ (,,,,,,,,,,,,,,,,)
        <$> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp
        <*> getRlp

-- -------------------------------------------------------------------------- --
-- Scalars
--
-- We use indiviual type classes so that we can use the type to short cut some
-- logic.

instance RLP Word8 where
    getRlp = label "Word8" $ int <$> getRlpN64

    putRlp x
        | x == 0 = put8 128
        | x < 128 = put8 x
        | otherwise = put8 129 <> putBe x

    {-# INLINEABLE getRlp #-}
    {-# INLINEABLE putRlp #-}

instance RLP Word16 where
    getRlp = label "Word16" $ int <$> getRlpN64

    putRlp x
        | x < pow256 1 = putRlp @Word8 (int x)
        | otherwise = let p = putBe x in put8 (128 + int (byteCount p)) <> p

    {-# INLINEABLE getRlp #-}
    {-# INLINEABLE putRlp #-}

instance RLP Word32 where
    getRlp = label "Word32" $ int <$> getRlpN64

    putRlp x
        | x < pow256 1 = putRlp @Word8 (int x)
        | otherwise = let p = putBe x in put8 (128 + int (byteCount p)) <> p

    {-# INLINEABLE getRlp #-}
    {-# INLINEABLE putRlp #-}

instance RLP Word64 where
    getRlp = label "Word64" $ int <$> getRlpN64

    putRlp x
        | x < pow256 1 = putRlp @Word8 (int x)
        | otherwise = let p = putBe x in put8 (128 + int (byteCount p)) <> p

    {-# INLINEABLE getRlp #-}
    {-# INLINEABLE putRlp #-}

-- | If RLP is used to encode a scalar, defined only as a non-negative integer
-- (in \(N\), or in \(N_x\) for any \(x\)), it must be encoded as the shortest
-- byte array whose big-endian interpretation is the scalar.
--
instance RLP Natural where
    getRlp = label "Natural" getRlpN

    putRlp x
        | lp == 1 && x < 128 = p
        | lp < 56 = put8 (int $ 128 + lp) <> p
        | lp <= maxBound @Word64 = put8 (int $ 183 + byteCount belp) <> belp <> p
        | otherwise = error $ "Byte arrays containing 2^64 or more bytes cannot be encoded: " <> show lp
      where
        p = putBe x
        belp = putBe lp

        lp :: Word64
        lp = int $ byteCount p

    {-# INLINEABLE getRlp #-}
    {-# INLINEABLE putRlp #-}
--    {-# SPECIALIZE instance RLP Natural #-}

instance (KnownSigned l, KnownSigned u, Show a, Real a, RLP a) => RLP (Checked l u a) where
    putRlp a = putRlp (unchecked a)
    getRlp = label "Checked" $ checked <$> getRlp >>= \case
        Right a -> return a
        Left e -> fail $ show e
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}
    {-# SPECIALIZE instance RLP (Checked ('P 0) ('P 115792089237316195423570985008687907853269984665640564039457584007913129639936) Natural) #-}

-- -------------------------------------------------------------------------- --
-- Checked Scalars


-- GHC currently (as of version 9.6.2) does require the constructor to be in
-- scope for GeneralizedNewtypeDeriving. This will be fixed in upcoming
-- versions.
--
-- Cf. https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10625
--
-- The following will be supported in future versions of GHC:
--
-- deriving newtype instance KnownNat (2^n-1) RLP (Checked.WordN n)
-- deriving newtype instance KnownNat (2^n-1) RLP (Checked.WordN8 n)
--
instance KnownNat (2^n-1) => RLP (Checked.WordN n) where
    putRlp = putRlp @(Checked ('P 0) ('P (2^n-1)) Natural) . unsafeCoerce
    getRlp = unsafeCoerce <$> getRlp @(Checked ('P 0) ('P (2^n-1)) Natural)

instance KnownNat (2^n-1) => RLP (Checked.WordN8 n) where
    putRlp = putRlp @(Checked.WordN n) . unsafeCoerce
    getRlp = unsafeCoerce <$> getRlp @(Checked.WordN n)

-- -------------------------------------------------------------------------- --
-- RLP Encoding Primitives

-- | RLP encoding for 'B.ByteString'.
--
-- If the value to be serialised is a byte array, the RLP serialisation takes
-- one of three forms:
--
putRlpB :: B.ByteString -> Put
putRlpB x

    -- If the byte array contains solely a single byte and that single byte is
    -- less than 128, then the input is exactly equal to the output.
    | lx == 1 && B.head x < 128 = put8 (B.head x)

    -- If the byte array contains fewer than 56 bytes, then the output is equal
    -- to the input prefixed by the byte equal to the length of the byte array
    -- plus 128.
    | lx < 56 = put8 (int $ 128 + lx) <> put x

    -- Otherwise, the output is equal to the input, provided that it contains
    -- fewer than \(2^64\) bytes, prefixed by the minimal-length byte array which
    -- when interpreted as a big-endian integer is equal to the length of the
    -- input byte array, which is itself prefixed by the number of bytes
    -- required to faithfully encode this length value plus 183.
    | lx <= maxBound @Word64 = put8 (int $ 183 + byteCount belx) <> belx <> put x


    -- Byte arrays containing 2^64 or more bytes cannot be encoded.
    | otherwise = error $ "Byte arrays containing 2^64 or more bytes cannot be encoded: " <> show lx
  where
    belx = putBe lx

    lx :: Word64
    lx = int $ B.length x
{-# INLINEABLE putRlpB #-}

-- | RLP encoding for 'BS.ShortByteString'.
--
putRlpBShort :: BS.ShortByteString -> Put
putRlpBShort x
    | lx == 1 && h x < 127 = put8 (h x)
    | lx < 56 = put8 (int $ 128 + lx) <> putShort x
    | lx <= maxBound @Word64 = put8 (int $ 183 + byteCount belx) <> belx <> putShort x
    | otherwise = error $ "Byte arrays containing 2^64 or more bytes cannot be encoded: " <> show lx
  where
    h = head . BS.unpack
    belx = putBe lx

    lx :: Word64
    lx = int $ BS.length x
{-# INLINEABLE putRlpBShort #-}

-- | RLP encoding for (possibly empty) lists and sequences.
--
-- If instead, the value to be serialised is a sequence of other items then
-- the RLP serialisation takes one of two forms:
--
putRlpL :: [Put] -> Put
putRlpL l

    -- If the concatenated serialisations of each contained item is less than 56
    -- bytes in length, then the output is equal to that concatenation prefixed
    -- by the byte equal to the length of this byte array plus 192.
    | ls < 56 = putBe (192 + ls) <> s

    -- Otherwise, the output is equal to the concatenated serialisations,
    -- provided that they contain fewer than \(2^64\) bytes, prefixed by the
    -- minimal-length byte array which when interpreted as a big-endian integer
    -- is equal to the length of the concatenated serialisations byte array,
    -- which is itself prefixed by the number of bytes required to faithfully
    -- encode this length value plus 247.
    | byteCount s < 2^(64 :: Int) =  putBe (247 + byteCount bels) <> bels <> s

    -- Sequences whose concatenated serialized items contain \(2^64\) or more
    -- bytes cannot be encoded.
    | otherwise = error "Sequences whose concatenated serialized items contain 2^64 or more bytes cannot be encoded."

  where
    s = mconcat l
    ls = byteCount s
    bels = putBe ls
{-# INLINE putRlpL #-}

-- -------------------------------------------------------------------------- --
-- RLP Decoding Primitives

-- | Check if the next encoded value is a byte array or scalar.
--
isB :: Get Bool
isB = label "isB" $ Get $ (< 192) <$> lookAhead getWord8
{-# INLINE isB #-}

-- | Check if the next encoded value is a list or sequence.
--
isL :: Get Bool
isL = label "isL" $ Get $ (>= 192) <$> lookAhead getWord8
{-# INLINE isL #-}

-- | RLP decoding function for 'B.ByteString' values of known length
--
-- This function fails fast if the input doesn't match the expected length which
-- can protect from some kinds of DOS attacks.
--
getRlpBSize :: Word64 -> Get B.ByteString
getRlpBSize n = label "getRlpBSize" $ Get getWord8 >>= go
  where
    go x
        | n == 1 && x < 128 = pure (B.singleton x)
        | n == int (x - 128) && x < 184 = Get $ getByteString (int $ x - 128)
        | x < 192 = do
            l <- getBe64 (int $ x - 183)
            unless (l == n) $
                fail $ "Unexpected size of input. Expected: " <> show n <> ". Actual " <> show l
            Get $! getByteString (int l)
        | otherwise = fail $ "invalid start byte for byte array encoding: " <> show x
{-# INLINEABLE getRlpBSize #-}

-- | RLP decoding function for 'B.ByteString' values
--
getRlpB :: Get B.ByteString
getRlpB = label "getRlpB" $ Get getWord8 >>= go
  where
    go x
        | x < 128 = pure (B.singleton x)
        | x < 184 = Get $ getByteString (int $ x - 128)
        | x < 192 = getBe64 (int $ x - 183) >>= Get . getByteString . int
        | otherwise = fail $ "invalid start byte for byte array encoding: " <> show x
{-# INLINEABLE getRlpB #-}

-- | RLP decoding function for 'Word64'.
--
getRlpN64 :: Get Word64
getRlpN64 = label "getRlpN64" $ Get getWord8 >>= go
  where
    go x
        | x == 0 = fail $ "invalid start byte for scalar encoding: " <> show x
        | x < 128 = return $ int x
        | x < 137 = getBe64 (x - 128)
        | x < 192 = fail $ "input number with " <> show (x - 128) <> " bytes too large for Word64 decoding"
        | otherwise = fail $ "invalid start byte for scalar encoding: " <> show x
{-# INLINEABLE getRlpN64 #-}

-- | RLP decoding function for arbitrary length unsigned scalar values.
--
getRlpN :: Get Natural
getRlpN = label "getRlpN" $ Get getWord8 >>= go
  where
    go x
        | x == 0 = fail $ "invalid start byte for scalar encoding: " <> show x
        | x < 128 = return $ int x
        | x < 137 = int <$> getBe64 (x - 128)
        | x < 192 = getBe (int x - 128)
        | otherwise = fail $ "invalid start byte for scalar encoding: " <> show x
{-# INLINEABLE getRlpN #-}

-- | RLP decoding for nested (and possibly empty) lists and sequences
--
getRlpL :: Get a -> Get a
getRlpL inner = label "getRlpL" $ Get getWord8 >>= go
  where
    go x
        | x < 192 = fail $ "invalid start byte for byte list encoding: " <> show x
        | x < 248 = isolate_ (int $ x - 192) inner
        | otherwise = do
            a <- getBe64 (int $ x - 247)
            isolate_ (int a) inner

    isolate_ n (Get f) = Get (isolate n f)
{-# INLINEABLE getRlpL #-}

-- | RLP decoding for nested (and possibly empty) lists and sequences
--
-- Takes a predicate about the size. If the predicate isn't satisfied the
-- parse fails fast.
--
getRlpLSize :: (Int -> Bool) -> Get a -> Get a
getRlpLSize p inner = label "getRlpLSize" $ Get getWord8 >>= go
  where
    go x
        | x < 192 = fail $ "invalid start byte for byte list encoding: " <> show x
        | x < 248 = isolate_ (int $ x - 192) inner
        | otherwise = do
            a <- getBe64 (int $ x - 247)
            isolate_ (int a) inner

    isolate_ n (Get f)
        | p n = Get (isolate n f)
        | otherwise = fail $ "unexpected length: " <> show n
{-# INLINEABLE getRlpLSize #-}

-- -------------------------------------------------------------------------- --
-- RLP Encoded Data
--
-- This is useful for iteratively encoding or decoding RLP encoded data. It
-- allows to "drop out" of an encoding and reuse the intermediate result later.
-- This is, for instance, used for enoding literal nodes in the Patricia Trie.

-- newtype RlpEncoded = RlpEncoded { _getRlpEncoded :: Put }
--     deriving (Show)
--
-- instance RLP RlpEncoded where
--     putRlp = _getRlp
--     getRlp = putRlpByteString

-- -------------------------------------------------------------------------- --
-- RLP Tree

-- | Simple tree data structure over byte arrays. Usefull for decoding
-- dynamically typed data from RLP.
--
data Tree
    = Leaf B.ByteString
    | Node [Tree]
    deriving (Show, Eq, Ord)

instance RLP Tree where
    putRlp = putRlpTree
    getRlp = getRlpTree
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- | Encode a 'Tree' to RLP.
--
putRlpTree :: Tree -> Put
putRlpTree (Leaf x) = putRlpB x
putRlpTree (Node x) = putRlpL $ putRlp <$> x
{-# INLINE putRlpTree #-}

-- | Decode a 'Tree' from RLP.
--
getRlpTree :: Get Tree
getRlpTree = label "getRlpTree" $ isB >>= \case
    True -> Leaf <$> getRlpB
    False -> Node <$> getRlpL go
  where
    -- the first argument is total number of bytes
    go :: Get [Tree]
    go = end >>= \case
        True -> return []
        False -> (:) <$> getRlpTree <*> go
{-# INLINE getRlpTree #-}

-- -------------------------------------------------------------------------- --
-- Utils

putRlpLazyByteString :: RLP a => a -> BL.ByteString
putRlpLazyByteString = putLazyByteString . putRlp
{-# INLINE putRlpLazyByteString #-}

putRlpByteString :: RLP a => a -> B.ByteString
putRlpByteString = putByteString . putRlp
{-# INLINE putRlpByteString #-}

putLazyByteString :: Put -> BL.ByteString
putLazyByteString = BB.toLazyByteString . builder
{-# INLINE putLazyByteString #-}

putByteString :: Put -> B.ByteString
putByteString = BL.toStrict . BB.toLazyByteString . builder
{-# INLINE putByteString #-}
