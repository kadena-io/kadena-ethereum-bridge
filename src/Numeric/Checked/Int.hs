{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Numeric.Checked.Int
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Numeric.Checked.Int
(
-- * Checked Int Types
  IntN
, intN

-- ** Checked Int types for sizes that are multiples of 8
, IntN8
, intN8

-- * Common Type Aliases
, Int8
, int8
, Int16
, int16
, Int32
, int32
, Int64
, int64
, Int128
, int128
, Int256
, int256
, Int512
, int512
) where

import Data.Aeson

import GHC.Stack
import GHC.TypeNats

#if !MIN_VERSION_base(4,16,0)
import Numeric.Natural
#endif

-- internal modules

import Ethereum.Utils

import Numeric.Checked

-- -------------------------------------------------------------------------- --
-- IntN

-- | IntN values.
--
newtype IntN (n :: Natural) = IntN (Checked ('N (2^(n-1))) ('P (2^(n-1)-1)) Integer)
    deriving (Show, Eq)
    deriving newtype (Ord, ToJSON)

deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1)) => Num (IntN n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1)) => Enum (IntN n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1)) => Real (IntN n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1)) => Integral (IntN n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1)) => FromJSON (IntN n)
-- deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1)) => RLP (IntN n)

deriving via (Checked ('N (2^(n-1))) ('P (2^(n-1)-1)) Integer)
    instance ToJSON (HexQuantity (IntN n))
deriving via (Checked ('N (2^(n-1))) ('P (2^(n-1)-1)) Integer)
    instance (KnownNat (2^(n-1)-1), KnownNat (2^(n-1))) => FromJSON (HexQuantity (IntN n))

intN
    :: forall n a
    . HasCallStack
    => KnownNat (2^(n-1))
    => KnownNat (2^(n-1)-1)
    => Integral a
    => a
    -> IntN n
intN = IntN . unsafeChecked . fromIntegral
{-# INLINE intN #-}

-- -------------------------------------------------------------------------- --
-- IntN8

newtype IntN8 n = IntN8 (IntN (CheckMult8 n))
    deriving (Show, Eq)
    deriving newtype (Ord, ToJSON)

deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1), Mod n 8 ~ 0) => Num (IntN8 n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1), Mod n 8 ~ 0) => Enum (IntN8 n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1), Mod n 8 ~ 0) => Real (IntN8 n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1), Mod n 8 ~ 0) => Integral (IntN8 n)
deriving newtype instance (KnownNat (2^(n-1)), KnownNat (2^(n-1)-1), Mod n 8 ~ 0) => FromJSON (IntN8 n)
-- deriving newtype instance (KnownNat (2^n-1), Mod n 8 ~ 0) => RLP (IntN8 n)

intN8
    :: forall n a
    . HasCallStack
    => KnownNat (2^(n-1))
    => KnownNat (2^(n-1)-1)
    => Mod n 8 ~ 0
    => Integral a
    => a
    -> IntN8 n
intN8 = IntN8 . intN @n
{-# INLINE intN8 #-}

-- -------------------------------------------------------------------------- --
-- Common Type Aliases

type Int8 = IntN 8
type Int16 = IntN 16
type Int32 = IntN 32
type Int64 = IntN 64
type Int128 = IntN 128
type Int256 = IntN 256
type Int512 = IntN 512

int8 :: Integral a => a -> Int8
int8 = intN

int16 :: Integral a => a -> Int16
int16 = intN

int32 :: Integral a => a -> Int32
int32 = intN

int64 :: Integral a => a -> Int64
int64 = intN

int128 :: Integral a => a -> Int128
int128 = intN

int256 :: Integral a => a -> Int256
int256 = intN

int512 :: Integral a => a -> Int512
int512 = intN

