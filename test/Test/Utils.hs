{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.Utils
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Utils
(
-- * Content addressed sets of key value pairs
  ContentAddressed(..)

-- * Hex Encoding
, d16
, dj
, unsafeEitherDecode
, toNibbles

-- * JSON decoding
, eitherDecodeString
, decodeHexTestCase
, decodeJsonTestCase

-- * Roundtrip Tests
, rlpRoundtrip
, rlpRoundtripTest
, jsonRpcRoundtrip
, jsonRpcRoundtripTest
) where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Function
import qualified Data.List as L

import Test.QuickCheck
import Test.Tasty.HUnit

-- internal modules

import Ethereum.HP
import Ethereum.RLP

-- -------------------------------------------------------------------------- --
-- Arbitary ContentAddressed Sets of Key Value Pairs

newtype ContentAddressed a b = ContentAddressed { getContentAddressed :: [(a,b)] }
    deriving (Show, Eq)

instance (Eq a, Arbitrary a, Arbitrary b) => Arbitrary (ContentAddressed a b) where
    arbitrary = ContentAddressed . L.nubBy ((==) `on` fst)
        <$> arbitrary

-- -------------------------------------------------------------------------- --
-- Hex Encoding

d16 :: B.ByteString -> B.ByteString
d16 = either error id . decode16
{-# INLINE d16 #-}

-- Decode strings using the respective FromJSON instance
--
dj :: FromJSON a => B.ByteString -> a
dj x = unsafeEitherDecode ("\"" <> x <> "\"")

unsafeEitherDecode :: FromJSON a => B.ByteString -> a
unsafeEitherDecode x = case eitherDecodeStrict x of
    Left e -> error $ "failed to decode test case: " <> e
    Right a -> a

-- -------------------------------------------------------------------------- --
-- JSON Decoding

eitherDecodeString :: FromJSON a => String -> Either String a
eitherDecodeString = eitherDecodeStrict . B8.pack
{-# INLINE eitherDecodeString #-}

decodeHexTestCase :: HasCallStack => B.ByteString -> IO B.ByteString
decodeHexTestCase hdr = case decode16 hdr of
    Left e -> assertFailure $ "d16: failed to decode hex encoded test case: " <> e
    Right x -> return x

decodeJsonTestCase
    :: forall a
    . HasCallStack
    => FromJSON a
    => Eq a
    => Show a
    => String
    -> IO a
decodeJsonTestCase x = case eitherDecodeString @a x of
    Left e -> assertFailure $ "Failed to JSON decode test case: " <> e
    Right a -> return a

-- -------------------------------------------------------------------------- --
-- Roundtrip Tests

rlpRoundtrip
    :: forall a
    . RLP a
    => Eq a
    => Show a
    => B.ByteString
    -> Assertion
rlpRoundtrip x = do
    decVal <- case get @a getRlp x of
        Left e -> assertFailure $ "Failed to RLP decode during first roundtrip: " <> e
        Right a -> return a
    let encVal = BL.toStrict $ BB.toLazyByteString $ builder $ putRlp @a decVal
    assertEqual "rlpRoundtrip: original encoded value and roundtrip value don't match" x encVal

    decVal' <- case get getRlp encVal of
        Left e -> assertFailure $ "Failed to RLP decode during second roundtrip: " <> e
        Right a -> return a
    assertEqual "rlpRoundtrip: orignal decoded value and roundtrip value don't match" decVal decVal'

rlpRoundtripTest
    :: forall a
    . RLP a
    => Eq a
    => Show a
    => B.ByteString
    -> Assertion
rlpRoundtripTest x = decodeHexTestCase x >>= rlpRoundtrip @a

jsonRpcRoundtrip
    :: forall a
    . HasCallStack
    => FromJSON a
    => ToJSON a
    => Eq a
    => Show a
    => a
    -> Assertion
jsonRpcRoundtrip val = do
    val' <- case eitherDecode @a (encode val) of
        Left e -> assertFailure $ "Failed to JSON decode during roundtrip: " <> e
        Right x -> return x
    assertEqual "jsonRpcRoundtrip: original and roundtrip value don't match" val val'

jsonRpcRoundtripTest
    :: forall a
    . HasCallStack
    => FromJSON a
    => ToJSON a
    => Eq a
    => Show a
    => String
    -> Assertion
jsonRpcRoundtripTest x = decodeJsonTestCase @a x >>= jsonRpcRoundtrip

