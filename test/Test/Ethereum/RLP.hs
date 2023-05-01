{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
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
module Test.Ethereum.RLP
( tests
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Typeable
import Data.Word

import GHC.TypeLits

#if !MIN_VERSION_base(4,16,0)
import Numeric.Natural
#endif

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Ethereum.HP
import Ethereum.Misc
import Ethereum.RLP

import Numeric.Checked hiding (check)

import Test.Orphans ()
import Test.Utils

import Test.Ethereum.HP ({- Arbitrary FlaggedNibbles -})

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "RLP"
    [ simpleTests
    , roundtripTests
    , otherTestCases
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
        , check @Word256
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
        , check @(Checked ('P 0) ('P (2^255)) Word256)
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
        , check @[Word256]
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
        , check @[((B.ByteString,Word64,Word256,Word8), [Natural])]
        , check @[((B.ByteString,BytesN 15,Word8), [Natural])]
        ]
    , testGroup "Ethereum Types"
        [ check @FlaggedNibbles
        ]
    ]
  where
    check :: forall a . Typeable a => Show a => Eq a => RLP a => Arbitrary a => TestTree
    check = testProperty (show (typeRep $ Proxy @a)) (roundtrip @a)

    checkOnce :: forall a . Typeable a => Show a => Eq a => RLP a => Arbitrary a => TestTree
    checkOnce = testProperty (show (typeRep $ Proxy @a)) (once $ roundtrip @a)

    roundtrip :: Show a => Eq a => RLP a => Arbitrary a => a -> Property
    roundtrip a = Right a === getLazy getRlp (BB.toLazyByteString (builder (putRlp a)))

-- -------------------------------------------------------------------------- --
--

otherTestCases :: TestTree
otherTestCases = testGroup "other test cases" $ checkCase <$> otherCases

data Case = forall a . RLP a => Case
    { _caseName :: !String
    , _caseIn :: a
    , _caseOut :: !B.ByteString
    }

checkCase :: Case -> TestTree
checkCase (Case n i o) = testProperty n $ putRlpByteString i === o

otherCases :: [Case]
otherCases =
    [ Case
        { _caseName = "emptystring"
        , _caseIn = B8.pack ""
        , _caseOut = d16 "80"
        }
    , Case
        { _caseName = "bytestring00"
        , _caseIn = "\x0000" :: B.ByteString
        , _caseOut = d16 "00"
        }
    , Case
        { _caseName = "bytestring01"
        , _caseIn = "\x0001" :: B.ByteString
        , _caseOut = d16 "01"
        }
    , Case
        { _caseName = "bytestring7F"
        , _caseIn = "\x007f" :: B.ByteString
        , _caseOut = d16 "7f"
        }
    , Case
        { _caseName = "shortstring"
        , _caseIn = "dog" :: B.ByteString
        , _caseOut = d16 "83646f67"
        }
    , Case
        { _caseName = "shortstring2"
        , _caseIn = "Lorem ipsum dolor sit amet, consectetur adipisicing eli" :: B.ByteString
        , _caseOut = d16 "b74c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e7365637465747572206164697069736963696e6720656c69"
        }
    , Case
        { _caseName = "longstring"
        , _caseIn = "Lorem ipsum dolor sit amet, consectetur adipisicing elit" :: B.ByteString
        , _caseOut = d16 "b8384c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e7365637465747572206164697069736963696e6720656c6974"
        }
    , Case
        { _caseName = "longstring2"
        , _caseIn = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur mauris magna, suscipit sed vehicula non, iaculis faucibus tortor. Proin suscipit ultricies malesuada. Duis tortor elit, dictum quis tristique eu, ultrices at risus. Morbi a est imperdiet mi ullamcorper aliquet suscipit nec lorem. Aenean quis leo mollis, vulputate elit varius, consequat enim. Nulla ultrices turpis justo, et posuere urna consectetur nec. Proin non convallis metus. Donec tempor ipsum in mauris congue sollicitudin. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Suspendisse convallis sem vel massa faucibus, eget lacinia lacus tempor. Nulla quis ultricies purus. Proin auctor rhoncus nibh condimentum mollis. Aliquam consequat enim at metus luctus, a eleifend purus egestas. Curabitur at nibh metus. Nam bibendum, neque at auctor tristique, lorem libero aliquet arcu, non interdum tellus lectus sit amet eros. Cras rhoncus, metus ac ornare cursus, dolor justo ultrices metus, at ullamcorper volutpat" :: B.ByteString
        , _caseOut = d16 "b904004c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e73656374657475722061646970697363696e6720656c69742e20437572616269747572206d6175726973206d61676e612c20737573636970697420736564207665686963756c61206e6f6e2c20696163756c697320666175636962757320746f72746f722e2050726f696e20737573636970697420756c74726963696573206d616c6573756164612e204475697320746f72746f7220656c69742c2064696374756d2071756973207472697374697175652065752c20756c7472696365732061742072697375732e204d6f72626920612065737420696d70657264696574206d6920756c6c616d636f7270657220616c6971756574207375736369706974206e6563206c6f72656d2e2041656e65616e2071756973206c656f206d6f6c6c69732c2076756c70757461746520656c6974207661726975732c20636f6e73657175617420656e696d2e204e756c6c6120756c74726963657320747572706973206a7573746f2c20657420706f73756572652075726e6120636f6e7365637465747572206e65632e2050726f696e206e6f6e20636f6e76616c6c6973206d657475732e20446f6e65632074656d706f7220697073756d20696e206d617572697320636f6e67756520736f6c6c696369747564696e2e20566573746962756c756d20616e746520697073756d207072696d697320696e206661756369627573206f726369206c756374757320657420756c74726963657320706f737565726520637562696c69612043757261653b2053757370656e646973736520636f6e76616c6c69732073656d2076656c206d617373612066617563696275732c2065676574206c6163696e6961206c616375732074656d706f722e204e756c6c61207175697320756c747269636965732070757275732e2050726f696e20617563746f722072686f6e637573206e69626820636f6e64696d656e74756d206d6f6c6c69732e20416c697175616d20636f6e73657175617420656e696d206174206d65747573206c75637475732c206120656c656966656e6420707572757320656765737461732e20437572616269747572206174206e696268206d657475732e204e616d20626962656e64756d2c206e6571756520617420617563746f72207472697374697175652c206c6f72656d206c696265726f20616c697175657420617263752c206e6f6e20696e74657264756d2074656c6c7573206c65637475732073697420616d65742065726f732e20437261732072686f6e6375732c206d65747573206163206f726e617265206375727375732c20646f6c6f72206a7573746f20756c747269636573206d657475732c20617420756c6c616d636f7270657220766f6c7574706174"
        }
    , Case
        { _caseName = "zero"
        , _caseIn = 0 :: Natural
        , _caseOut = d16 "80"
        }
    , Case
        { _caseName = "smallint"
        , _caseIn = 1 :: Natural
        , _caseOut = d16 "01"
        }
    , Case
        { _caseName = "smallint2"
        , _caseIn = 16 :: Natural
        , _caseOut = d16 "10"
        }
    , Case
        { _caseName = "smallint3"
        , _caseIn = 79 :: Natural
        , _caseOut = d16 "4f"
        }
    , Case
        { _caseName = "smallint4"
        , _caseIn = 127 :: Natural
        , _caseOut = d16 "7f"
        }
    , Case
        { _caseName = "mediumint1"
        , _caseIn = 128 :: Natural
        , _caseOut = d16 "8180"
        }
    , Case
        { _caseName = "mediumint2"
        , _caseIn = 1000 :: Natural
        , _caseOut = d16 "8203e8"
        }
    , Case
        { _caseName = "mediumint3"
        , _caseIn = 100000 :: Natural
        , _caseOut = d16 "830186a0"
        }
    , Case
        { _caseName = "mediumint4"
        , _caseIn = 83729609699884896815286331701780722 :: Natural
        , _caseOut = d16 "8f102030405060708090a0b0c0d0e0f2"
        }
    , Case
        { _caseName = "mediumint5"
        , _caseIn = 105315505618206987246253880190783558935785933862974822347068935681 :: Natural
        , _caseOut = d16 "9c0100020003000400050006000700080009000a000b000c000d000e01"
        }
    , Case
        { _caseName = "bigint"
        , _caseIn = 115792089237316195423570985008687907853269984665640564039457584007913129639936 :: Natural
        , _caseOut = d16 "a1010000000000000000000000000000000000000000000000000000000000000000"
        }
    , Case
        { _caseName = "emptylist"
        , _caseIn = [] :: [B.ByteString]
        , _caseOut = d16 "c0"
        }
    , Case
        { _caseName = "stringlist"
        , _caseIn = [ "dog", "god", "cat" ] :: [B.ByteString]
        , _caseOut = d16 "cc83646f6783676f6483636174"
        }
    , Case
        { _caseName = "multilist"
        , _caseIn = ("zw", [ 4 ], 1) :: (B.ByteString, [Natural], Natural)
        , _caseOut = d16 "c6827a77c10401"
        }
    , Case
        { _caseName = "shortListMax1"
        , _caseIn = ["asdf", "qwer", "zxcv", "asdf","qwer", "zxcv", "asdf", "qwer", "zxcv", "asdf", "qwer"] :: [B.ByteString]
        , _caseOut = d16 "f784617364668471776572847a78637684617364668471776572847a78637684617364668471776572847a78637684617364668471776572"
        }
    , Case
        { _caseName = "longList1"
        , _caseIn =
            [ ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            ] :: [[B.ByteString]]
        , _caseOut = d16 "f840cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376"
        }
    , Case
        { _caseName = "longList2"
        , _caseIn =
            [ ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            , ["asdf","qwer","zxcv"]
            ] :: [[B.ByteString]]
        , _caseOut = d16 "f90200cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376cf84617364668471776572847a786376"
        }
    , Case
        { _caseName = "listsoflists"
        , _caseIn = [ [ [], [] ], [] ] :: [[[()]]]
        , _caseOut = d16 "c4c2c0c0c0"
        }
    , Case
        { _caseName = "listsoflists2"
        , _caseIn = [ [], [[]], [ [], [[]] ] ] :: [[[[()]]]]
        , _caseOut = d16 "c7c0c1c0c3c0c1c0"
        }
    , Case
        { _caseName = "dictTest1"
        , _caseIn =
            [ ("key1", "val1")
            , ("key2", "val2")
            , ("key3", "val3")
            , ("key4", "val4")
            ] :: [(B.ByteString, B.ByteString)]
        , _caseOut = d16 "ecca846b6579318476616c31ca846b6579328476616c32ca846b6579338476616c33ca846b6579348476616c34"
        }
    ]

