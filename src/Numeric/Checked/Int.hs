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
-- * IntN
  IntN
, intN
, IntN8
, intN8
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

