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
-- Module: Word
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Numeric.Checked.Word
(
-- * Checked Word Types
  WordN
, wordN

-- ** Checked word types for sizes that are multiples of 8
, WordN8
, wordN8

-- * Common Type Aliases
, Word8
, word8
, Word16
, word16
, Word32
, word32
, Word64
, word64
, Word128
, word128
, Word256
, word256
, Word512
, word512
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
-- WordN

-- | WordN values.
--
newtype WordN (n :: Natural) = WordN (Checked ('P 0) ('P (2^n - 1)) Natural)
    deriving (Show, Eq)
    deriving newtype (Ord, ToJSON)

deriving newtype instance KnownNat (2^n-1) => Num (WordN n)
deriving newtype instance KnownNat (2^n-1) => Enum (WordN n)
deriving newtype instance KnownNat (2^n-1) => Real (WordN n)
deriving newtype instance KnownNat (2^n-1) => Integral (WordN n)
deriving newtype instance KnownNat (2^n-1) => FromJSON (WordN n)
deriving newtype instance KnownNat (2^n-1) => Bounded (WordN n)

deriving via (Checked ('P 0) ('P (2^n-1)) Natural)
    instance ToJSON (HexQuantity (WordN n))
deriving via (Checked ('P 0) ('P (2^n-1)) Natural)
    instance KnownNat (2^n-1) => FromJSON (HexQuantity (WordN n))

wordN
    :: forall n a
    . HasCallStack
    => KnownNat n
    => KnownNat (2^n-1)
    => Integral a
    => a
    -> WordN n
wordN = WordN . unsafeChecked . fromIntegral
{-# INLINE wordN #-}

-- -------------------------------------------------------------------------- --
-- WordN8

newtype WordN8 n = WordN8 (WordN (CheckMult8 n))
    deriving (Show, Eq)
    deriving newtype (Ord, ToJSON)

deriving newtype instance (KnownNat (2^n-1), Mod n 8 ~ 0) => Num (WordN8 n)
deriving newtype instance (KnownNat (2^n-1), Mod n 8 ~ 0) => Enum (WordN8 n)
deriving newtype instance (KnownNat (2^n-1), Mod n 8 ~ 0) => Real (WordN8 n)
deriving newtype instance (KnownNat (2^n-1), Mod n 8 ~ 0) => Integral (WordN8 n)
deriving newtype instance (KnownNat (2^n-1), Mod n 8 ~ 0) => FromJSON (WordN8 n)

wordN8
    :: forall n a
    . HasCallStack
    => KnownNat n
    => KnownNat (2^n-1)
    => Mod n 8 ~ 0
    => Integral a
    => a
    -> WordN8 n
wordN8 = WordN8 . wordN @n
{-# INLINE wordN8 #-}

-- -------------------------------------------------------------------------- --
-- Common Type Aliases

type Word8 = WordN 8
type Word16 = WordN 16
type Word32 = WordN 32
type Word64 = WordN 64
type Word128 = WordN 128
type Word256 = WordN 256
type Word512 = WordN 512

word8 :: Integral a => a -> Word8
word8 = wordN

word16 :: Integral a => a -> Word16
word16 = wordN

word32 :: Integral a => a -> Word32
word32 = wordN

word64 :: Integral a => a -> Word64
word64 = wordN

word128 :: Integral a => a -> Word128
word128 = wordN

word256 :: Integral a => a -> Word256
word256 = wordN

word512 :: Integral a => a -> Word512
word512 = wordN

