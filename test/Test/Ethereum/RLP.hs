{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Test.Ethereum.RLP
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Test.Ethereum.RLP
( tests
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Typeable
import Data.Word

import GHC.TypeLits

import Numeric.Natural

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Ethereum.Misc
import Ethereum.RLP

import Numeric.Checked hiding (check)

import Test.Orphans ()

-- -------------------------------------------------------------------------- --
--

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "RLP"
    [ simpleTests
    , roundtripTests
    ]

-- -------------------------------------------------------------------------- --
-- Examples from https://eth.wiki/en/fundamentals/rlp

simpleTests :: TestTree
simpleTests = testGroup "simple RLP tests"
    [ check @B.ByteString "dog" "\x83\&dog"
    , check @BS.ShortByteString "dog" "\x83\&dog"
    , check @(BytesN 3) "dog" "\x83\&dog"
    , check @[B.ByteString] ["cat", "dog"] "\xc8\x83\&cat\x83\&dog"
    , check @[BytesN 3] ["cat", "dog"] "\xc8\x83\&cat\x83\&dog"
    , check @[BS.ShortByteString] ["cat", "dog"] "\xc8\x83\&cat\x83\&dog"
    , check @(B.ByteString, B.ByteString) ("cat", "dog") "\xc8\x83\&cat\x83\&dog"
    , check @(B.ByteString, BS.ShortByteString) ("cat", "dog") "\xc8\x83\&cat\x83\&dog"
    , check @(BS.ShortByteString, BS.ShortByteString) ("cat", "dog") "\xc8\x83\&cat\x83\&dog"
    , check @(BytesN 3, BytesN 3) ("cat", "dog") "\xc8\x83\&cat\x83\&dog"
    , check @B.ByteString "" "\x80"
    , check @BS.ShortByteString "" "\x80"
    , check @(BytesN 0) "" "\x80"
    , check @[B.ByteString] [] "\xc0"
    , check () "\xc0"
    , check @Word8 0 "\x80"
    , check @Word16 0 "\x80"
    , check @Word32 0 "\x80"
    , check @Word64 0 "\x80"
    , check @Natural 0 "\x80"
    , check @B.ByteString "\x00" "\x00"
    , check @B.ByteString "\x0f" "\x0f"
    , check @B.ByteString "\x04\x00" "\x82\x04\x00"
    , check @(BytesN 1) "\x00" "\x00"
    , check @(BytesN 1) "\x0f" "\x0f"
    , check @(BytesN 2) "\x04\x00" "\x82\x04\x00"
    , check (Node [ Node [], Node [ Node [] ], Node [ Node [], Node [ Node []] ] ]) "\xc7\xc0\xc1\xc0\xc3\xc0\xc1\xc0"
    , check @B.ByteString "Lorem ipsum dolor sit amet, consectetur adipisicing elit" "\xb8\x38\&Lorem ipsum dolor sit amet, consectetur adipisicing elit"
    , check @BS.ShortByteString "Lorem ipsum dolor sit amet, consectetur adipisicing elit" "\xb8\x38\&Lorem ipsum dolor sit amet, consectetur adipisicing elit"
    ]
  where
    check :: Typeable a => Show a => Eq a => RLP a => a -> BL.ByteString -> TestTree
    check a b = testGroup (show a <> " @" <> show (typeOf a)) $
        [ testProperty "encode" $ checkEncode a b
        , testProperty "decode" $ checkDecode a b
        ]

    checkEncode :: Show a => Eq a => RLP a => a -> BL.ByteString -> Property
    checkEncode a b = BB.toLazyByteString (builder (putRlp a)) === b

    checkDecode :: Show a => Eq a => RLP a => a -> BL.ByteString -> Property
    checkDecode a b = property $ Right a === getLazy getRlp b

roundtripTests :: TestTree
roundtripTests = testGroup "roundtrips"
    [ testGroup "scalars"
        [ check @Word8
        , check @Word16
        , check @Word32
        , check @Word64
        , check @(Small Word64)
        , check @(Large Word64)
        , check @Natural
        , check @(Small Natural)
        ]
    , testGroup "checked"
        [ check @(Checked ('P 0) ('P 128) Word8)
        , check @(Checked ('P 0) ('P (2^15)) Word16)
        , check @(Checked ('P 0) ('P (2^31)) Word32)
        , check @(Checked ('P 0) ('P (2^63)) Word64)
        , check @(Checked ('P 0) ('P (2^70)) Natural)
        ]
    , testGroup "ByteString"
        [ check @B.ByteString
        , check @BS.ShortByteString
        ]
    , testGroup "BytesN"
        [ check @(BytesN 0)
        , check @(BytesN 1)
        , check @(BytesN 2)
        , check @(BytesN 8)
        , check @(BytesN 21)
        , check @(BytesN 32)
        , check @(BytesN 1023)
        , check @(BytesN 1024)
        , check @(BytesN 1025)
        , checkOnce @(BytesN (2^20+17))
        ]
    , testGroup "tuples"
        [ check @()
        , check @(Word8,Word16)
        , check @(B.ByteString,Word64)
        , check @(B.ByteString,Word64,Word8)
        ]
    , testGroup "lists"
        [ check @[Word8]
        , check @[Word16]
        , check @[Word32]
        , check @[Word64]
        , check @[Natural]
        , check @[B.ByteString]
        , check @[BS.ShortByteString]
        , check @[BytesN 31]
        , check @[BytesN 32]
        ]
    , testGroup "misc"
        [ check @((B.ByteString,Word64,Word8), Natural)
        , check @((B.ByteString,Word64,Word8), [Natural])
        , check @[((B.ByteString,Word64,Word8), [Natural])]
        , check @[((B.ByteString,BytesN 15,Word8), [Natural])]
        ]
    ]
  where
    check :: forall a . Typeable a => Show a => Eq a => RLP a => Arbitrary a => TestTree
    check = testProperty (show (typeRep $ Proxy @a)) (roundtrip @a)

    checkOnce :: forall a . Typeable a => Show a => Eq a => RLP a => Arbitrary a => TestTree
    checkOnce = testProperty (show (typeRep $ Proxy @a)) (once $ roundtrip @a)

    roundtrip :: Show a => Eq a => RLP a => Arbitrary a => a -> Property
    roundtrip a = Right a === getLazy getRlp (BB.toLazyByteString (builder (putRlp a)))

