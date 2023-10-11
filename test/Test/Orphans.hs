{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Numeric.Checked.Word qualified as Checked

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

-- Work around a bug in QuickChecks instance `Integral a => (Small a)` for
-- types that don't include negative values.
--
-- TODO make QuickCheck PR for this
--
instance {-# OVERLAPPING #-} Arbitrary (Small Natural) where
  arbitrary = fmap Small arbitrarySizedNatural
  shrink (Small x) = fmap Small (shrinkIntegral x)

instance (KnownNat (2^n-1)) => Arbitrary (Checked.WordN n) where
    arbitrary = arbitrarySizedBoundedIntegral
    {-# INLINE arbitrary #-}

-- | See comment on instance for `(Small Natural)`.
--
instance {-# OVERLAPPING #-} (KnownNat (2^n-1)) => Arbitrary (Small (Checked.WordN n)) where
    arbitrary = fmap Small arbitrarySizedBoundedIntegral
    shrink (Small x) = fmap Small (shrinkIntegral x)
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

