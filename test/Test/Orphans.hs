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
-- TODO
--
module Test.Orphans
(
) where

import qualified Data.ByteString.Short as BS

import GHC.Exts (proxy#)
import GHC.TypeLits

import Numeric.Natural

import Test.QuickCheck

-- internal modules

import Ethereum.Misc
import Ethereum.RLP

import Numeric.Checked

-- -------------------------------------------------------------------------- --
-- Orphans
--
-- TODO move in separate module
-- TODO instance for non-integral types

instance (Show a, Arbitrary a, Integral a, KnownSigned l, KnownSigned u) => Arbitrary (Checked l u a) where
    arbitrary = arbitrarySizedBoundedIntegral
    {-# INLINE arbitrary #-}

instance KnownNat n => Arbitrary (BytesN n) where
    arbitrary = bytesN . BS.pack <$> vector (fromIntegral $ natVal' (proxy# @n)) >>= \case
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

