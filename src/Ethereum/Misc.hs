{-# LANGUAGE BangPatterns #-}
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
-- TODO
--
module Ethereum.Misc
(
-- * Byte Arrays of known length
  BytesN
, _getBytesN
, bytesN
, replicateN

-- * Word256
, Word256
, word256

-- * Misc Types
, Address(..)
, Beneficiary(..)
, Nonce(..)

-- * Scalars
, Number(..)
, Difficulty(..)
, GasLimit(..)
, GasUsed(..)
, ExtraData(..)
, Timestamp(..)

-- * Hashes
, Keccak256Hash(..)
, BlockHash(..)
, ParentHash(..)
, OmmersHash(..)
, StateRoot(..)
, TransactionsRoot(..)
, ReceiptsRoot(..)
, MixHash(..)
, TransactionHash(..)
, keccak256

-- * Bloom Filters
, Bloom(..)
, mkBloom
) where

import Control.Monad.ST

import Crypto.Hash

import Data.Aeson
import Data.Aeson.Internal
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as BSI
import Data.Primitive.ByteArray
import qualified Data.Primitive.ByteArray as BA
import Data.String
import qualified Data.Text as T
import Data.Word

import GHC.TypeNats

import Numeric.Natural

import GHC.Exts (proxy#)


-- internal modules

import Ethereum.RLP
import Ethereum.Utils

import Numeric.Checked

-- -------------------------------------------------------------------------- --
-- Fixed Size Byte Arrays

newtype BytesN (n :: Nat) = BytesN BS.ShortByteString
    deriving (Show, Eq)
    deriving newtype (Ord, IsString)

_getBytesN :: BytesN n -> BS.ShortByteString
_getBytesN (BytesN x) = x
{-# INLINE _getBytesN #-}

bytesN
    :: forall n
    . KnownNat n
    => BS.ShortByteString
    -> Either String (BytesN n)
bytesN b
    | int (BS.length b) == natVal' (proxy# @n) = Right $ BytesN b
    | otherwise = Left "bytesN: initialized with wrong number of bytes"
{-# INLINE bytesN #-}

instance KnownNat n => RLP (BytesN n) where
    putRlp (BytesN b) = putRlp b
    getRlp = BytesN . BS.toShort <$> getRlpBSize (int $ natVal' (proxy# @n))
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance ToJSON (HexBytes (BytesN n)) where
    toEncoding (HexBytes (BytesN a)) = toEncoding $ HexBytes a
    toJSON (HexBytes (BytesN a)) = toJSON $ HexBytes a
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

-- TODO: add label for parser
instance KnownNat n => FromJSON (HexBytes (BytesN n)) where
    parseJSON v = go <?> Key ("HexBytes (BytesN " <> T.pack (show (natVal' (proxy# @n))))
      where
        go = do
            (HexBytes a) <- parseJSON v
            case bytesN a of
                Right x -> return $ HexBytes x
                Left e -> fail e
    {-# INLINE parseJSON #-}

replicateN :: forall n . KnownNat n => Word8 -> BytesN n
replicateN a = BytesN $ BS.toShort $ B.replicate (int $ natVal' (proxy# @n)) a

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
-- TODO: check length when decoding
--
newtype Address = Address (BytesN 20)
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes (BytesN 20))
    deriving FromJSON via (HexBytes (BytesN 20))

newtype Beneficiary = Beneficiary Address
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON)
    deriving FromJSON via (JsonCtx "Beneficiary" Address)

newtype Nonce = Nonce (BytesN 8)
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes (BytesN 8))
    deriving FromJSON via (JsonCtx "Nonce" (HexBytes (BytesN 8)))

-- -------------------------------------------------------------------------- --
-- Scalar Values

newtype Number = Number Natural
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype GasUsed = GasUsed Natural
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype Difficulty = Difficulty Natural
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype GasLimit = GasLimit Natural
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

newtype ExtraData = ExtraData BS.ShortByteString -- 32 bytes or less
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes BS.ShortByteString)
    deriving FromJSON via (HexBytes BS.ShortByteString)

newtype Timestamp = Timestamp Word64
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Word64)
    deriving FromJSON via (JsonCtx "Timestamp" (HexQuantity Word64))

-- -------------------------------------------------------------------------- --
-- Hashes

newtype Keccak256Hash = Keccak256Hash (BytesN 32)
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

newtype BlockHash = BlockHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

newtype ParentHash = ParentHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

newtype OmmersHash = OmmersHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

newtype StateRoot = StateRoot Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

newtype TransactionsRoot = TransactionsRoot Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

newtype TransactionHash = TransactionHash Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

newtype ReceiptsRoot = ReceiptsRoot Keccak256Hash
    deriving (Show, Eq)
    deriving newtype (RLP, ToJSON, FromJSON)

newtype MixHash = MixHash (BytesN 32)
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

keccak256 :: B.ByteString -> Keccak256Hash
keccak256 = Keccak256Hash
    . BytesN
    . digestToShortByteString
    . hash @_ @Keccak_256
{-# INLINE keccak256 #-}

-- -------------------------------------------------------------------------- --
-- Bloom Filter

newtype Bloom = Bloom (BytesN 256)
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes (BytesN 256))
    deriving FromJSON via (HexBytes (BytesN 256))

-- | Creates a bloom filter iteratively
--
-- TODO for long inputs lists or long input values it is more efficient to
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
    go arr bytes = do
        setBloomBit (m 0)
        setBloomBit (m 2)
        setBloomBit (m 4)
      where
        !h = let (Keccak256Hash x) = keccak256 bytes in _getBytesN x

        -- Get bloom bit position by extracting lower 11 bits at index i
        -- TODO: check endianess
        m :: Int -> Int
        m i = int (BS.index h i) + 256 * (int (0x7 .&. BS.index h (i + 1)))
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
