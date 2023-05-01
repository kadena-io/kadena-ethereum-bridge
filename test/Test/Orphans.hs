{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Test.Orphans
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Orphans
(
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS

import GHC.TypeLits

#if !MIN_VERSION_base(4,16,0)
import Numeric.Natural
#endif

import Test.QuickCheck

import GHC.Exts (Proxy#, proxy#)

-- internal modules

import Ethereum.HP.Internal
import Ethereum.Misc
import Ethereum.RLP

import Numeric.Checked

-- -------------------------------------------------------------------------- --
-- Orphans

instance (Show a, Arbitrary a, Integral a, KnownSigned l, KnownSigned u) => Arbitrary (Checked l u a) where
    arbitrary = arbitrarySizedBoundedIntegral
    {-# INLINE arbitrary #-}

instance KnownNat n => Arbitrary (BytesN n) where
    arbitrary = bytesN . BS.pack <$> vector (fromIntegral $ natVal' (proxy# :: Proxy# n)) >>= \case
        Left _ -> error "Arbitrary BytesN: failed to generate arbitary value. This is a bug"
        Right x -> return x
    {-# INLINE arbitrary #-}

instance RLP a => RLP (Small a) where
    putRlp = putRlp . getSmall
    getRlp = Small <$> getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance RLP a => RLP (Large a) where
    putRlp = putRlp . getLarge
    getRlp = Large <$> getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- Work around a bug in QuickCheck
--
-- TODO make QuickCheck PR for this
--
instance {-# OVERLAPPING #-} Arbitrary (Small Natural) where
  arbitrary = fmap Small arbitrarySizedNatural
  shrink (Small x) = map Small (shrinkIntegral x)

instance Arbitrary Word256 where
    arbitrary = word256 <$> choose @Integer (0, 2^(256 :: Int) - 1)
    {-# INLINE arbitrary #-}

-- -------------------------------------------------------------------------- --
-- HP

instance Arbitrary Nibbles where
    arbitrary = do
        bs <- B.pack <$> arbitrary
        o <- chooseInt (0, max 0 (2 * B.length bs - 1))
        l <- chooseInt (0, 2 * B.length bs - o)
        return $ Nibbles o l bs
    {-# INLINE arbitrary #-}

instance Arbitrary FlaggedNibbles where
    arbitrary = FlaggedNibbles <$> arbitrary <*> arbitrary
    {-# INLINE arbitrary #-}

