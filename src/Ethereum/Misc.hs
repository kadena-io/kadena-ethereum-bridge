{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Ethereum.Misc
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Ethereum.Misc
(
-- * Bytes class
  Bytes(..)

-- * Byte Arrays of known length
, BytesN
, _getBytesN
, bytesN
, unsafeBytesN
, replicateN
, appendN
, emptyN
, nullN
, indexN
, reverseN
, encodeLeN
, encodeBeN

-- * Word256
, Word256
, word256

-- * Misc Types
, Address(..)
, Beneficiary(..)
, Nonce(..)

-- * Scalars
, BlockNumber(..)
, BlockSize(..)
, Difficulty(..)
, GasLimit(..)
, GasUsed(..)
, ExtraData(..)
, Timestamp(..)
, BlockDepth(..)

-- * Hashes
, Keccak256Hash(..)
, _getKeccak256Hash
, Keccak512Hash(..)
, _getKeccak512Hash
, BlockHash(..)
, TruncatedBlockHash(..)
, ParentHash(..)
, OmmersHash(..)
, StateRoot(..)
, TransactionsRoot(..)
, ReceiptsRoot(..)
, MixHash(..)
, TransactionHash(..)
, keccak256
, keccak512

-- * Bloom Filters
, Bloom(..)
, mkBloom

-- * Backward compatibility with bytestring <0.10.10
, useAsCStringLenBS
, packCStringLenBS
) where

import Control.Monad.ST

import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as BSI
import qualified Data.ByteString.Unsafe as BU
import Data.Coerce
import Data.Hashable (Hashable)
import Data.Hash.Keccak
import Data.Primitive.ByteArray
import qualified Data.Primitive.ByteArray as BA
import Data.String
import Data.Word

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String (CStringLen)

import GHC.Stack
import qualified GHC.TypeLits as L
import GHC.TypeNats

#if !MIN_VERSION_base(4,18,0)
import Numeric.Natural (Natural)
#endif

import GHC.Exts (Proxy#, proxy#)


-- internal modules

import Ethereum.RLP
import Ethereum.Utils

import Numeric.Checked

-- -------------------------------------------------------------------------- --
-- Backward compatibility with bytestring <0.10.10

useAsCStringLenBS :: BS.ShortByteString -> (CStringLen -> IO a) -> IO a
#if ! MIN_VERSION_bytestring(0,10,10)
useAsCStringLenBS = B.useAsCStringLen . BS.fromShort
#else
useAsCStringLenBS = BS.useAsCStringLen
#endif
{-# INLINE useAsCStringLenBS #-}

packCStringLenBS :: CStringLen -> IO BS.ShortByteString
#if ! MIN_VERSION_bytestring(0,10,10)
packCStringLenBS = fmap BS.toShort . B.packCStringLen
#else
packCStringLenBS = BS.packCStringLen
#endif
{-# INLINE packCStringLenBS #-}

-- -------------------------------------------------------------------------- --
-- Bytes

-- | Class of types that have a low-level representation as bytestring
--
class Bytes a where

    -- | Provide a pure bytestring representation of a value
    --
    bytes :: a -> B.ByteString

    -- | Index a byte in the byte representation of a value
    --
    index :: a -> Int -> Word8

    -- | Provide a pointer to a copy of the bytes. The pointed to memory may be
    -- modified with out affecting the original value. However, the memory is
    -- freed when the function exits and must not be used after that.
    --
    withPtr :: a -> (Ptr Word8 -> Int -> IO b) -> IO b

    -- | The pointer that is given to the callback must not be used to modify
    -- the memory. Modifying the memory would break referential transparency.
    --
    -- While implementations may actually create a copy of the bytes the user
    -- must not rely on that.
    --
    unsafeWithPtr :: a -> (Ptr Word8 -> Int -> IO b) -> IO b

    index b = B.index (bytes b)

    withPtr b f = B.useAsCStringLen (bytes b) $ \(ptr, l) ->
        f (castPtr ptr) l

    unsafeWithPtr b f = BU.unsafeUseAsCStringLen (bytes b) $ \(ptr, l) ->
        f (castPtr ptr) l

    {-# INLINEABLE withPtr #-}
    {-# INLINEABLE unsafeWithPtr #-}
    {-# INLINEABLE index #-}
    {-# MINIMAL bytes #-}

instance Bytes B.ByteString where
    bytes = id
    {-# INLINE bytes #-}

instance Bytes BL.ByteString where
    bytes = BL.toStrict
    index b = BL.index b . int
    {-# INLINE bytes #-}
    {-# INLINE index #-}

instance Bytes BS.ShortByteString where
    bytes = BS.fromShort
    index = BS.index
    withPtr b f = useAsCStringLenBS b $ \(ptr, l) -> f (castPtr ptr) l
    unsafeWithPtr b f = useAsCStringLenBS b $ \(ptr, l) -> f (castPtr ptr) l
    {-# INLINE bytes #-}
    {-# INLINE index #-}
    {-# INLINE withPtr #-}
    {-# INLINE unsafeWithPtr #-}

-- -------------------------------------------------------------------------- --
-- Fixed Size Byte Arrays

newtype BytesN (n :: Nat) = BytesN BS.ShortByteString
    deriving (Eq)
    deriving newtype (Ord, IsString, Hashable, Bytes)

instance KnownNat n => Show (BytesN n) where
    show (BytesN bs) = "BytesN"
        <> " " <> show (natVal' (proxy# :: Proxy# n))
        <> " 0x" <> B8.unpack (B16.encode $ BS.fromShort bs)

_getBytesN :: BytesN n -> BS.ShortByteString
_getBytesN (BytesN x) = x
{-# INLINE _getBytesN #-}

bytesN
    :: forall n
    . KnownNat n
    => BS.ShortByteString
    -> Either String (BytesN n)
bytesN b
    | int (BS.length b) == natVal' (proxy# :: Proxy# n) = Right $ BytesN b
    | otherwise = Left "bytesN: initialized with wrong number of bytes"
{-# INLINEABLE bytesN #-}

unsafeBytesN
    :: forall n
    . HasCallStack
    => KnownNat n
    => BS.ShortByteString
    -> BytesN n
unsafeBytesN b = case bytesN b of
    Right x -> x
    Left e -> error $ "Ethereum.Misc.unsafeBytesN: " <> e
{-# INLINEABLE unsafeBytesN #-}

instance KnownNat n => RLP (BytesN n) where
    putRlp (BytesN b) = putRlp b
    getRlp = BytesN . BS.toShort <$> getRlpBSize (int $ natVal' (proxy# :: Proxy# n))
    {-# INLINEABLE putRlp #-}
    {-# INLINEABLE getRlp #-}

    {-# SPECIALIZE instance RLP (BytesN 8) #-}
    {-# SPECIALIZE instance RLP (BytesN 16) #-}
    {-# SPECIALIZE instance RLP (BytesN 32) #-}
    {-# SPECIALIZE instance RLP (BytesN 64) #-}

instance ToJSON (HexBytes (BytesN n)) where
    toEncoding (HexBytes (BytesN a)) = toEncoding $ HexBytes a
    toJSON (HexBytes (BytesN a)) = toJSON $ HexBytes a
    {-# INLINEABLE toEncoding #-}
    {-# INLINEABLE toJSON #-}

    {-# SPECIALIZE instance ToJSON (HexBytes (BytesN 8)) #-}
    {-# SPECIALIZE instance ToJSON (HexBytes (BytesN 16)) #-}
    {-# SPECIALIZE instance ToJSON (HexBytes (BytesN 32)) #-}
    {-# SPECIALIZE instance ToJSON (HexBytes (BytesN 64)) #-}

instance KnownNat n => FromJSON (HexBytes (BytesN n)) where
    parseJSON v = go <?> Key ("HexBytes (BytesN " <> fromString (show (natVal' (proxy# :: Proxy# n))))
      where
        go = do
            (HexBytes a) <- parseJSON v
            case bytesN a of
                Right x -> return $ HexBytes x
                Left e -> fail e
    {-# INLINEABLE parseJSON #-}

    {-# SPECIALIZE instance FromJSON (HexBytes (BytesN 8)) #-}
    {-# SPECIALIZE instance FromJSON (HexBytes (BytesN 16)) #-}
    {-# SPECIALIZE instance FromJSON (HexBytes (BytesN 32)) #-}
    {-# SPECIALIZE instance FromJSON (HexBytes (BytesN 64)) #-}

replicateN :: forall n . KnownNat n => Word8 -> BytesN n
replicateN a = BytesN $ BS.toShort $ B.replicate (int $ natVal' (proxy# :: Proxy# n)) a
{-# INLINEABLE replicateN #-}
{-# SPECIALIZE replicateN :: Word8 -> BytesN 8 #-}
{-# SPECIALIZE replicateN :: Word8 -> BytesN 16 #-}
{-# SPECIALIZE replicateN :: Word8 -> BytesN 32 #-}
{-# SPECIALIZE replicateN :: Word8 -> BytesN 64 #-}

appendN :: BytesN a -> BytesN b -> BytesN (a + b)
appendN (BytesN a) (BytesN b) = BytesN (a <> b)
{-# INLINEABLE appendN #-}

emptyN :: BytesN 0
emptyN = BytesN ""
{-# INLINEABLE emptyN #-}

nullN :: BytesN n -> Bool
nullN (BytesN "") = True
nullN _ = False

indexN :: BytesN n -> Int -> Word8
indexN (BytesN b) = BS.index b
{-# INLINEABLE indexN #-}

instance KnownNat n => Storable (BytesN n) where
    sizeOf _ = int (L.natVal' (proxy# :: Proxy# n))
    alignment _ = 1

    peek ptr = BytesN <$> packCStringLenBS (castPtr ptr, int (L.natVal' (proxy# :: Proxy# n)))

    poke ptr a = unsafeWithPtr a $ \aPtr _ ->
        copyBytes aPtr (castPtr ptr) (int (L.natVal' (proxy# :: Proxy# n)))

    {-# INLINEABLE sizeOf #-}
    {-# INLINEABLE alignment #-}
    {-# INLINEABLE peek #-}
    {-# INLINEABLE poke #-}

    {-# SPECIALIZE instance Storable (BytesN 8) #-}
    {-# SPECIALIZE instance Storable (BytesN 16) #-}
    {-# SPECIALIZE instance Storable (BytesN 32) #-}
    {-# SPECIALIZE instance Storable (BytesN 64) #-}

encodeLeN :: forall n . KnownNat n => Natural -> BytesN n
encodeLeN x = unsafeBytesN $ toShortByteString $ encodeLe (int $ intVal_ @n) x
{-# INLINEABLE encodeLeN #-}
{-# SPECIALIZE encodeLeN :: Natural -> BytesN 8 #-}
{-# SPECIALIZE encodeLeN :: Natural -> BytesN 16 #-}
{-# SPECIALIZE encodeLeN :: Natural -> BytesN 32 #-}
{-# SPECIALIZE encodeLeN :: Natural -> BytesN 64 #-}

encodeBeN :: forall n . KnownNat n => Natural -> BytesN n
encodeBeN x = unsafeBytesN $ toShortByteString $ encodeBe (int $ intVal_ @n) x
{-# INLINEABLE encodeBeN #-}
{-# SPECIALIZE encodeBeN :: Natural -> BytesN 8 #-}
{-# SPECIALIZE encodeBeN :: Natural -> BytesN 16 #-}
{-# SPECIALIZE encodeBeN :: Natural -> BytesN 32 #-}
{-# SPECIALIZE encodeBeN :: Natural -> BytesN 64 #-}

reverseN :: BytesN n -> BytesN n
reverseN (BytesN b) = BytesN $ BS.toShort $ B.reverse $ BS.fromShort b
{-# INLINEABLE reverseN #-}

-- -------------------------------------------------------------------------- --
-- Word256

-- | Word256 values.
--
newtype Word256 = Word256 (Checked ('P 0) ('P (2^256)) Natural)
  deriving (Show, Eq)
  deriving newtype (Ord, Num, Enum, Real, Integral, RLP, ToJSON, FromJSON)

deriving via (Checked ('P 0) ('P (2^256)) Natural) instance ToJSON (HexQuantity Word256)
deriving via (Checked ('P 0) ('P (2^256)) Natural) instance FromJSON (HexQuantity Word256)

word256 :: Integral a => a -> Word256
word256 a
    | a >= 0 && a < pow256 32 = Word256 (int a)
    | otherwise = error "word256: value out of bounds"
{-# INLINE word256 #-}

-- -------------------------------------------------------------------------- --
-- Misc

-- | 160 Bit (20 bytes) Address
--
newtype Address = Address (BytesN 20)
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable)
    deriving ToJSON via (HexBytes (BytesN 20))
    deriving FromJSON via (HexBytes (BytesN 20))

newtype Beneficiary = Beneficiary Address
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, Bytes, Storable)
    deriving FromJSON via (JsonCtx "Beneficiary" Address)

newtype Nonce = Nonce (BytesN 8)
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable)
    deriving ToJSON via (HexBytes (BytesN 8))
    deriving FromJSON via (JsonCtx "Nonce" (HexBytes (BytesN 8)))

-- -------------------------------------------------------------------------- --
-- Scalar Values

newtype BlockNumber = BlockNumber Natural
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype BlockSize = BlockSize Natural
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype GasUsed = GasUsed Natural
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype Difficulty = Difficulty Natural
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype GasLimit = GasLimit Natural
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype ExtraData = ExtraData BS.ShortByteString -- 32 bytes or less
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes)
    deriving ToJSON via (HexBytes BS.ShortByteString)
    deriving FromJSON via (HexBytes BS.ShortByteString)

newtype Timestamp = Timestamp Word64
    deriving (Show, Eq)
    deriving newtype (RLP, Storable)
    deriving ToJSON via (HexQuantity Word64)
    deriving FromJSON via (JsonCtx "Timestamp" (HexQuantity Word64))

newtype BlockDepth = BlockDepth Natural
    deriving (Show, Eq, Ord, Enum, Real, Integral, Num)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

-- -------------------------------------------------------------------------- --
-- Hashes

newtype Keccak256Hash = Keccak256Hash (BytesN 32)
    deriving (Show, Eq, Ord)
    deriving newtype (RLP, Bytes, Storable, Hashable)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

_getKeccak256Hash :: Keccak256Hash -> BytesN 32
_getKeccak256Hash (Keccak256Hash b) = b
{-# INLINE _getKeccak256Hash #-}

newtype Keccak512Hash = Keccak512Hash (BytesN 64)
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, Hashable)
    deriving ToJSON via (HexBytes (BytesN 64))
    deriving FromJSON via (HexBytes (BytesN 64))

_getKeccak512Hash :: Keccak512Hash -> BytesN 64
_getKeccak512Hash (Keccak512Hash b) = b
{-# INLINE _getKeccak512Hash #-}

newtype BlockHash = BlockHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

newtype TruncatedBlockHash = TruncatedBlockHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

newtype ParentHash = ParentHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

newtype OmmersHash = OmmersHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

newtype StateRoot = StateRoot Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

newtype TransactionsRoot = TransactionsRoot Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

newtype TransactionHash = TransactionHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

newtype ReceiptsRoot = ReceiptsRoot Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable, ToJSON, FromJSON)

-- | Intermidate mix hash. This is computed from the dataset and
-- is the input for the final POW computation: @serialize_hash(sha3_256(s+cmix))@.
--
-- It provides a way to verify that some minimal amount of work was spent on the
-- header, which can be used as DOS protection.
--
newtype MixHash = MixHash (BytesN 32)
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

keccak256 :: B.ByteString -> Keccak256Hash
keccak256 = Keccak256Hash
    . BytesN
    . coerce
    . hashByteString @Keccak256
{-# INLINEABLE keccak256 #-}

keccak512 :: B.ByteString -> Keccak512Hash
keccak512 = Keccak512Hash
    . BytesN
    . coerce
    . hashByteString @Keccak512
{-# INLINEABLE keccak512 #-}

-- -------------------------------------------------------------------------- --
-- Bloom Filter

newtype Bloom = Bloom (BytesN 256)
    deriving (Show, Eq)
    deriving newtype (RLP, Bytes, Storable)
    deriving ToJSON via (HexBytes (BytesN 256))
    deriving FromJSON via (HexBytes (BytesN 256))

-- | Creates a bloom filter iteratively
--
-- TODO for long input lists or long input values it is more efficient to
-- create bloom filters for chunks in parallel and to merge the filters.
--
mkBloom :: [B.ByteString] -> Bloom
mkBloom bs = runST $ do
    arr <- BA.newByteArray 256
    setByteArray @Word64 arr 0 4 0x0
    mapM_ (go arr) bs
    ByteArray a <- unsafeFreezeByteArray arr
    return $! Bloom $! BytesN $ BSI.SBS a
  where
    go :: forall s . MutableByteArray s -> B.ByteString -> ST s ()
    go arr b = do
        setBloomBit (m 0)
        setBloomBit (m 2)
        setBloomBit (m 4)
      where
        !h = let (Keccak256Hash x) = keccak256 b in _getBytesN x

        -- Get bloom bit position by extracting lower 11 bits at index i
        -- TODO: check endianess
        m :: Int -> Int
        m i = int (BS.index h i) + 256 * int (0x7 .&. BS.index h (i + 1))
        {-# INLINE m #-}

        -- Set bit in a Mutable ByteArray
        --
        setBloomBit :: Int -> ST s ()
        setBloomBit i = do
            cur <- readByteArray @Word8 arr off
            writeByteArray @Word8 arr off (bit pos .|. cur)
          where
            (!off, !pos) = quotRem i 8
        {-# INLINE setBloomBit #-}

