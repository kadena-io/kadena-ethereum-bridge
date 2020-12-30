{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Test.Ethereum.HP
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Ethereum.HP
( tests
, hexDigitTests
, nibbleTests
, hpTests
) where

import Data.Bifunctor
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Word

import qualified Test.QuickCheck as Q
import Test.QuickCheck hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

-- internal modules

import Ethereum.HP.Internal

import Test.Orphans ()

tests :: TestTree
tests = testGroup "HP"
    [ hexDigitTests
    , nibbleTests
    , hpTests
    ]

-- -------------------------------------------------------------------------- --
-- Hex Digits

hexDigitTests :: TestTree
hexDigitTests = testProperties "HexDigit"
    [ ("prop_upper", property prop_upper)
    , ("prop_lower", property prop_lower)
    , ("prop_hexDigits", property prop_hexDigits)
    ]

prop_upper :: Word8 -> Property
prop_upper x = getHexDigit (upper x) === shiftR x 4

prop_lower :: Word8 -> Property
prop_lower x = getHexDigit (lower x) === (x .&. 0x0f)

prop_hexDigits :: Word8 -> Property
prop_hexDigits x = hexDigits x === (upper x, lower x)

-- -------------------------------------------------------------------------- --
-- Nibbles

nibbleTests :: TestTree
nibbleTests = testProperties "Nibbles"
    [ ("prop_nsplitAt", property prop_nsplitAt)
    , ("prop_nrange", property prop_nrange)
    , ("prop_nhead", property prop_nhead)
    , ("prop_nhead_", property prop_nhead_)
    , ("prop_mempty", property prop_mempty)
    , ("prop_mempty2", property prop_mempty2)
    , ("prop_nalign", property prop_nalign)
    , ("prop_hp", prop_hp)
    ]

newtype NonEmptyNibbles = NonEmptyNibbles { getNonEmptyNibbles :: Nibbles }
    deriving (Show, Eq)

instance Arbitrary NonEmptyNibbles where
    arbitrary = do
        bytes <- B.pack . getNonEmpty <$> arbitrary
        o <- chooseInt (0, max 0 (2 * B.length bytes - 1))
        l <- chooseInt (1, 2 * B.length bytes - o)
        return $ NonEmptyNibbles $ Nibbles o l bytes
    {-# INLINE arbitrary #-}

prop_nsplitAt :: Int -> Nibbles -> Bool
prop_nsplitAt x ns = bimap checkNibbles checkNibbles (nsplitAt x ns) `seq` True

prop_nrange :: NonNegative Int -> NonNegative Int -> Nibbles -> Bool
prop_nrange (NonNegative l) (NonNegative u) ns = checkNibbles (nrange l u ns) `seq` True

prop_nhead :: NonEmptyNibbles -> Property
prop_nhead (NonEmptyNibbles n) = nhead n === nix n 0

prop_nhead_ :: NonEmptyNibbles -> Property
prop_nhead_ (NonEmptyNibbles n) = nhead_ n === nix_ n 0

prop_mempty :: Property
prop_mempty
    = (mempty === Nibbles 0 0 "")
    Q..&. (nlength mempty === 0)
    Q..&. (mempty @Nibbles <> mempty === mempty)

prop_mempty2 :: Nibbles -> Property
prop_mempty2 ns
    = ns <> mempty === ns
    Q..&. mempty <> ns === ns

prop_nalign :: Nibbles -> Property
prop_nalign ns = ns === nalign ns

-- -------------------------------------------------------------------------- --
-- HP Tests

hpTests :: TestTree
hpTests = testProperty "hp tests" prop_hp

prop_hp :: Property
prop_hp
    = (f False x1 === "10012345")
    Q..&. (f True x1 === "30012345")
    Q..&. (f False x2 === "00012345")
    Q..&. (f True x2 === "20012345")
  where
    x1 = nibbles (B.pack [ 0x00, 0x12, 0x34, 0x50 ]) 0 7
    x2 = nibbles (B.pack [ 0x01, 0x23, 0x45 ]) 0 6
    f b = B16.encode . toHp . FlaggedNibbles b
