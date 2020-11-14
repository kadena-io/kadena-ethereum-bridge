{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.Ethereum.Trie
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Test.Ethereum.Trie
( tests
) where

import Control.Monad

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import qualified Data.List as L

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck

-- internal modules

import Ethereum.Misc
import Ethereum.Trie

import Test.Orphans ()
import Test.Utils

-- -------------------------------------------------------------------------- --
-- Utils

kv :: Int -> (B.ByteString, B.ByteString)
kv i = (B.take i "abcde", "k" <> B8.pack (show i))

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "Trie"
    [ testCase "case0" $ runCase case0
    , testCase "case1" $ runCase case1
    , testCase "case2" $ runCase case2
    , testCase "case3" $ runCase case3
    , testCase "case4" $ runCase case4
    , testCase "case5" $ runCase case5
    , testCase "case6" $ runCase case6
    , testCase "case7" $ runCase case7
    , testCase "case8" $ runCase case8
    , testCase "case9" $ runCase case9
    , testCase "case10" $ runCase case10
    , testGroup "Subsequences of k0, k1, k2, k3, k4"
        $ (\c -> testCase (show c) $ runCase c) <$> cases
    , testProperty "quickcheck" prop_trie
    , rootTests
    ]

-- -------------------------------------------------------------------------- --
--

runCase :: [(B.ByteString, B.ByteString)] -> IO ()
runCase ps = do
    s <- mkHashMapStore
    t <- trie (_trieStoreAdd s) ps
    forM_ ps $ \p@(k, v) -> do
        r <- lookupTrie (_trieStoreLookup s) t k
        assertEqual ("pair " <> show p <> " exists in trie") (Just v) r

-- -------------------------------------------------------------------------- --
-- Test cases

case0, case1, case2, case3, case4, case5 :: [(B.ByteString, B.ByteString)]
case0 = []
case1 = [kv 0]
case2 = [kv 1]
case3 = [kv 2]
case4 = [kv 0, kv 1, kv 2]
case5 = [kv 2, kv 4]

case6 :: [(B.ByteString, B.ByteString)]
case6 =
    [ (d16 "04", "0")
    , ("", "0")
    , (d16 "0008" ,"0")
    ]

case7 :: [(B.ByteString, B.ByteString)]
case7 =
    [ ("", "0")
    , ("", "0")
    ]

case8 :: [(B.ByteString, B.ByteString)]
case8 =
    [ (mempty, "0")
    , (mempty, "0")
    ]

case9 :: [(B.ByteString, B.ByteString)]
case9 =
    [ (d16 "00", "0")
    , (d16 "10", "0")
    , (d16 "0000", "0")
    ]

-- | cf. https://eth.wiki/en/fundamentals/patricia-tree
--
case10 :: [(B.ByteString, B.ByteString)]
case10 =
    [ ("do", "verb")
    , ("dog", "puppy")
    , ("doge", "coin")
    , ("horse", "stallion")
    ]

cases :: [[(B.ByteString, B.ByteString)]]
cases = L.subsequences [kv 0, kv 1, kv 2, kv 3, kv 4]

-- -------------------------------------------------------------------------- --
-- QuickCheck

prop_trie :: ContentAddressed B.ByteString B.ByteString -> Property
prop_trie (ContentAddressed ps) = (all (not . B8.null . snd) ps) ==> monadicIO $ do
    s <- run mkHashMapStore
    t <- run $ trie (_trieStoreAdd s) ps
    forM_ ps $ \(k, v) -> do
        r <- run $ lookupTrie (_trieStoreLookup s) t k
        assert $ (Just v) == r

-- -------------------------------------------------------------------------- --
--

rootTests :: TestTree
rootTests = testGroup "other test cases" $ checkRoot <$> rootCases

data Case = Case
    { _caseName :: !String
    , _caseIn :: [(B.ByteString, B.ByteString)]
    , _caseOut :: !Keccak256Hash
    }
    deriving (Show)

checkRoot :: Case -> TestTree
checkRoot (Case n i o) = testCase n $ do
    s <- mkHashMapStore
    Trie t <- trie (_trieStoreAdd s) i
    assertEqual n o t

-- | https://github.com/ethereum/tests/blob/86098807850c6211042f6a35ad8a48fc6072e856/TrieTests/trieanyorder.json
--
rootCases :: [Case]
rootCases =
    [ Case -- X
        { _caseName = "emptyValues"
        , _caseIn =
            [ ("do", "verb")
            -- , ("ether", "wookiedoo")
            , ("horse", "stallion")
            -- , ("shaman", "horse")
            , ("doge", "coin")
            -- , ("ether", "")
            , ("dog", "puppy")
            -- , ("shaman", "")
            ]
        -- it seems that for the test data, setting a value to "" is equivalent to deleting the
        -- key from the trie
        , _caseOut = dj "0x5991bb8c6514148a29db676a14ac506cd2cd5775ace63c30a4fe457715e9ac84"
        }
    , Case -- X
        { _caseName = "insert-middle-leaf"
        , _caseIn =
            [ ("key1aa", "0123456789012345678901234567890123456789xxx")
            , ("key1", "0123456789012345678901234567890123456789Very_Long")
            , ("key2bb", "aval3")
            , ("key2", "short")
            , ("key3cc", "aval3")
            , ("key3","1234567890123456789012345678901")
            ]
        , _caseOut = dj "0xcb65032e2f76c48b82b5c24b3db8f670ce73982869d38cd39a624f23d62a9e89"
        }
    , Case -- +
        { _caseName = "singleItem"
        , _caseIn = [ ("A", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")]
        , _caseOut = dj "0xd23786fb4a010da3ce639d66d5e904a11dbc02746d1ce25029e53290cabf28ab"
        }
    , Case -- X
        { _caseName = "hex"
        , _caseIn =
            [ (d16 "0045", d16 "0123456789")
            , (d16 "4500", d16 "9876543210")
            ]
        , _caseOut = dj "0x285505fcabe84badc8aa310e2aae17eddc7d120aabec8a476902c8184b3a3503"
        }
    , Case -- +
        { _caseName = "foo"
        , _caseIn =
            [ ("foo", "bar")
            , ("food", "bass")
            ]
        , _caseOut = dj "0x17beaa1648bafa633cda809c90c04af50fc8aed3cb40d16efbddee6fdf63c4c3"
        }
    , Case -- X
        { _caseName = "smallValues"
        , _caseIn =
            [ ("be", "e")
            , ("dog", "puppy")
            , ("bed", "d")
            ]
        , _caseOut = dj "0x3f67c7a47520f79faa29255d2d3c084a7a6df0453116ed7232ff10277a8be68b"
        }
    , Case -- +
        { _caseName = "testy"
        , _caseIn =
            [ ("test", "test")
            , ("te", "testy")
            ]
        , _caseOut = dj "0x8452568af70d8d140f58d941338542f645fcca50094b20f3c3d8c3df49337928"
        }
    , Case -- X
        { _caseName = "puppy"
        , _caseIn =
            [ ("do", "verb")
            , ("horse", "stallion")
            , ("doge", "coin")
            , ("dog", "puppy")
            ]
        , _caseOut = dj "0x5991bb8c6514148a29db676a14ac506cd2cd5775ace63c30a4fe457715e9ac84"
        }
    , Case -- X
        { _caseName = "dogs"
        , _caseIn =
            [ ("doe", "reindeer")
            , ("dog", "puppy")
            , ("dogglesworth", "cat")
            ]
        , _caseOut = dj "0x8aad789dff2f538bca5d8ea56e8abe10f4c7ba3a5dea95fea4cd6e7c3a1168d3"
        }

    -- https://github.com/ethereum/tests/blob/develop/TrieTests/hex_encoded_securetrie_test.json
    -- NOTE: this is a "secure trie" (the input keys are hashes with keccak256 to get 64 bit)
    , Case -- X
        { _caseName = "test1"
        , _caseIn =
            [ ( dsec "a94f5374fce5edbc8e2a8697c15331677e6ebf0b"
              , d16 "f848018405f446a7a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
              )
            , ( dsec "095e7baea6a6c7c4c2dfeb977efac326af552d87"
              , d16 "f8440101a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a004bccc5d94f4d1f99aab44369a910179931772f2a5c001c3229f57831c102769"
              )
            , ( dsec "d2571607e241ecf590ed94b12d87c94babe36db6"
              , d16 "f8440180a0ba4b47865c55a341a4a78759bb913cd15c3ee8eaf30a62fa8d1c8863113d84e8a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
              )
            , ( dsec "62c01474f089b07dae603491675dc5b5748f7049"
              , d16 "f8448080a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
              )
            , ( dsec "2adc25665018aa1fe0e6bc666dac8fc2697ff9ba"
              , d16 "f8478083019a59a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a0c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
              )
            ]
        , _caseOut = dj "0x730a444e08ab4b8dee147c9b232fc52d34a223d600031c1e9d25bfc985cbd797"
        }

    -- https://github.com/ethereum/tests/blob/f5afc3963535be4b8bc4a8086f2bfb3c911d6517/TrieTests/trietest.json
    , Case -- +
        { _caseName = "branch-value-update"
        , _caseIn =
            -- [ ("abc", "123")
            [ ("abcd", "abcd")
            , ("abc", "abc")
            ]
        , _caseOut = dj "0x7a320748f780ad9ad5b0837302075ce0eeba6c26e3d8562c67ccc0f1b273298a"
        }
    ]
  where
    dj :: B.ByteString -> Keccak256Hash
    dj x = case eitherDecodeStrict ("\"" <> x <> "\"") of
        Left e -> error $ "failed to decode test case: " <> e
        Right a -> a

    dsec h = let Keccak256Hash b = keccak256 (d16 h) in BS.fromShort (_getBytesN b)

