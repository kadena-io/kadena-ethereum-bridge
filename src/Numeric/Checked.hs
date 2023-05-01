{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Numeric.Checked
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Bounded Numeric Types with Bounds Checks
--
module Numeric.Checked
(
-- * Typelevel Integers
  Signed(..)
, KnownSigned(..)

-- * Bound-Checked Arithmetic
, CheckedException(..)
, Checked
, KnownChecked
, check
, checked
, unsafeChecked
, unchecked
) where

import Control.Exception

import Data.Aeson
import Data.Bifunctor

import GHC.Stack
import GHC.TypeLits

import GHC.Exts (Proxy#, proxy#)

-- -------------------------------------------------------------------------- --
-- Utils

lower :: Real a => a -> Integer
lower = floor . toRational
{-# NOINLINE [1] lower #-}

{-# RULES
"lower/Integral" forall (a :: Integral a => a) . lower a = fromIntegral a
"lower/RealFrac" forall (a :: RealFrac a => a) . lower a = floor a
#-}

upper :: Real a => a -> Integer
upper = ceiling . toRational
{-# NOINLINE [1] upper #-}

{-# RULES
"upper/Integral" forall (a :: Integral a => a) . upper a = fromIntegral a
"upper/RealFrac" forall (a :: RealFrac a => a) . upper a = ceiling a
#-}

-- -------------------------------------------------------------------------- --
-- Typelevel Integers

data Signed = N Nat | P Nat

class KnownSigned (a :: Signed) where
    signedVal :: Proxy# a -> Integer
    signedVal' :: Integer
    signedVal' = signedVal (proxy# :: Proxy# a)
    {-# INLINE signedVal' #-}

    {-# MINIMAL signedVal #-}

instance KnownNat a => KnownSigned ('N a) where
    signedVal _ = negate $! natVal' (proxy# :: Proxy# a)
    {-# INLINE signedVal #-}

instance KnownNat a => KnownSigned ('P a) where
    signedVal _ = natVal' (proxy# :: Proxy# a)
    {-# INLINE signedVal #-}

-- -------------------------------------------------------------------------- --
-- Exceptions

data CheckedException
    = CheckedArithmeticUnderflow String
    | CheckedArithmeticOverflow String
    deriving (Show, Eq, Ord)

instance Exception CheckedException

-- -------------------------------------------------------------------------- --
-- Bound-Checked Arithmetic

newtype Checked (l :: Signed) (u :: Signed) a = Checked a
    deriving (Show)
    deriving newtype (Eq, Ord)

type KnownChecked l u a = (KnownSigned l, KnownSigned u, Real a, Show a)

unchecked :: Checked l u a -> a
unchecked (Checked a) = a
{-# INLINE unchecked #-}

check
    :: forall (l :: Signed) (u :: Signed) a
    . KnownSigned l
    => KnownSigned u
    => Real a
    => a
    -> Bool
check a = signedVal' @l <= lower a && upper a <= signedVal' @u
{-# INLINE check #-}

checked
    :: forall (l :: Signed) (u :: Signed) a
    . KnownSigned l
    => KnownSigned u
    => Real a
    => Show a
    => a
    -> Either CheckedException (Checked l u a)
checked a
    | signedVal' @l > lower a = Left $ CheckedArithmeticUnderflow $
        "Value " <> show a <> " is smaller than lower bound " <> show (signedVal' @l)
    | upper a > signedVal' @u = Left $ CheckedArithmeticOverflow $
        "Value " <> show a <> " is larger than upper bound " <> show (signedVal' @u)
    | otherwise = Right $ Checked a
{-# INLINE checked #-}

unsafeChecked
    :: forall (l :: Signed) (u :: Signed) a
    . HasCallStack
    => KnownSigned l
    => KnownSigned u
    => Real a
    => Show a
    => a
    -> Checked l u a
unsafeChecked = either throw id . checked
{-# INLINE unsafeChecked #-}

instance (KnownSigned l, KnownSigned u, Num a) => Bounded (Checked l u a) where
    minBound = Checked $ fromInteger $ signedVal' @l
    maxBound = Checked $ fromInteger $ signedVal' @u
    {-# INLINE minBound #-}
    {-# INLINE maxBound #-}

instance (KnownSigned l, KnownSigned u, Integral a, Show a, Enum a) => Enum (Checked l u a) where
    toEnum n = unsafeChecked $ fromIntegral n
    fromEnum (Checked n) = fromIntegral n
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}

instance (KnownSigned l, KnownSigned u, Show a, Real a) => Num (Checked l u a) where
    Checked a + Checked b = unsafeChecked $! a + b
    Checked a * Checked b = unsafeChecked $! a * b
    abs (Checked a) = unsafeChecked $! abs a
    signum (Checked a) = unsafeChecked $! signum a
    fromInteger = unsafeChecked . fromInteger
    negate (Checked a) = unsafeChecked $ negate a
    {-# INLINE (+) #-}
    {-# INLINE (*) #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}
    {-# INLINE negate #-}

instance (KnownSigned l, KnownSigned u, Show a, Real a) => Real (Checked l u a) where
    toRational (Checked a) = toRational a
    {-# INLINE toRational #-}

instance (KnownSigned l, KnownSigned u, Show a, Integral a) => Integral (Checked l u a) where
    toInteger (Checked a) = toInteger a
    quotRem (Checked a) (Checked b) = bimap unsafeChecked unsafeChecked (quotRem a b)
    divMod (Checked a) (Checked b) = bimap unsafeChecked unsafeChecked (divMod a b)
    quot (Checked a) (Checked b) = unsafeChecked $! quot a b
    rem (Checked a) (Checked b) = unsafeChecked $! rem a b
    div (Checked a) (Checked b) = unsafeChecked $! div a b
    mod (Checked a) (Checked b) = unsafeChecked $! mod a b
    {-# INLINE toInteger #-}
    {-# INLINE quotRem #-}
    {-# INLINE divMod #-}
    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE div #-}
    {-# INLINE mod #-}

instance ToJSON a => ToJSON (Checked l u a) where
    toJSON (Checked a) = toJSON a
    toEncoding (Checked a) = toEncoding a
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance (KnownSigned l, KnownSigned u, FromJSON a, Real a, Show a) => FromJSON (Checked l u a) where
    parseJSON v = do
        n <- parseJSON v
        case checked n of
            Left e -> fail $ show e
            Right x -> return x
    {-# INLINE parseJSON #-}
