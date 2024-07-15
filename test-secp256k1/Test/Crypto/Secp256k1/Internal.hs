{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Test.Crypto.Secp256k1.Internal
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Test.Crypto.Secp256k1.Internal
( tests
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Coerce
import Data.Hash.SHA3
import Data.Word (Word8)

import GHC.TypeNats

import System.Entropy

import Test.QuickCheck hiding (Fn)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- internal modules

import Crypto.Secp256k1.Internal
import Crypto.Secp256k1

-- -------------------------------------------------------------------------- --
-- Examples
-- -------------------------------------------------------------------------- --

_sk1, h1, r1, s1 :: Fn
_sk1 = fn 0xebb2c082fd7727890a28ac82f6bdf97bad8de9f5d7c9028692de1a255cad3e0f
h1 = fn 0x4b688df40bcedbe641ddb16ff0a1842d9c67ea1c3bf63f3e0471baa664531d1a
r1 = fn 0x241097efbf8b63bf145c8961dbdf10c310efbb3b2676bbc0f8b08505c9e2f795
s1 = fn 0x021006b7838609339e8b415a7f9acb1b661828131aef1ecbc7955dfb01f3ca0e
pkx1, pky1 :: Fp
pkx1 = fp 0x779dd197a5df977ed2cf6cb31d82d43328b790dc6b3b7d4437a427bd5847dfcd
pky1 = fp 0xe94b724a555b6d017bb7607c3e3281daf5b1699d6ef4124975c9237b917d426f
pk1 :: Point
pk1 = case maybePublicKey pkx1 pky1 of
    Just p -> p
    Nothing -> error "invalid key"

test_1_verify :: Property
test_1_verify = verify h1 r1 s1 pk1 === Right True

test_1_recover :: Property
test_1_recover = recoverPublicKey h1 r1 s1 False False === Just pk1

test_2_recover :: Property
test_2_recover = recoverPublicKey h1 r1 s1 True False =/= Just pk1

properties_example1 :: TestTree
properties_example1 = testGroup "example1"
    [ testProperty "test_1_verify" test_1_verify
    , testProperty "test_1_recover" test_1_recover
    , testProperty "test_2_recover" test_2_recover
    ]

_sk2, _k2, _h2, _r2, _s2 :: Fn
_sk2 = fn 0xebb2c082fd7727890a28ac82f6bdf97bad8de9f5d7c9028692de1a255cad3e0f
_k2 = fn 0x49a0d7b786ec9cde0d0721d72804befd06571c974b191efb42ecf322ba9ddd9a
_h2 = fn 0x4b688df40bcedbe641ddb16ff0a1842d9c67ea1c3bf63f3e0471baa664531d1a
_r2 = fn 0x241097efbf8b63bf145c8961dbdf10c310efbb3b2676bbc0f8b08505c9e2f795
_s2 = fn 0x021006b7838609339e8b415a7f9acb1b661828131aef1ecbc7955dfb01f3ca0e

-- -------------------------------------------------------------------------- --
-- Tests Tools
-- -------------------------------------------------------------------------- --
--
-- DO NOT use any of the following code in production.

genSecretKey :: IO Fn
genSecretKey = bytesToFn <$> getEntropy 32

genKey :: IO (Fn, Point)
genKey = do
    sk <- genSecretKey
    case getPublicKey sk of
        O -> error "invalid public key"
        pk -> return (sk, pk)

-- |
--
-- https://www.secg.org/sec1-v2.pdf, 4.1.3:
--
--     The publicly verifiable criteria that r may be conditioned to satisfy may
--     include that xR is uniquely recoverable from r in that only one of the
--     integers \(xR = r + jn\) for \(j ∈ {0, 1, 2, ..., h}\) represents a valid
--     x-coordinate of a multiple of G. For the recommended curves [SEC 2] with h =
--     1 and h = 2, the number of valid candidate x-coordinates is usually one, so
--     this is a vacuous check.
--
-- "Usually" here means something like always except for one out of \(2^{128}\).
-- However, in the context of a public blockchain, an attack may be able to
-- fabricate a respective signature and cause diverging behavior between
-- validating nodes with different implementations for handling this corner
-- case. Although, creating such an attack may be infeasible.
--
-- For the meaning of the y-parity bit see:
--
--    https://eips.ethereum.org/EIPS/eip-2098
--
-- For testing purposes it would be useful to implement (pure) deterministic
-- signing (https://www.rfc-editor.org/rfc/rfc6979#section-3)
--
sign
    :: Fn
        -- ^ secret key
    -> Fn
        -- ^ message digest
    -> IO (Fn, Fn, Bool, Bool)
        -- ^ (r, s, isOddY, isSecondKey)
sign sk e = do
    (k, Point xr yr) <- genKey
    let r = zConv xr
    if r == fn 0
      then sign sk e -- start over
      else do
        let s_ = invM k .* (e .+ r .* sk)
            s = if s_ > halfN then minusM s_ else s_
        if s == fn 0
          then sign sk e -- start over
          else do
            let isSecondKey = nat xr < (pC - nC)
                isOddY = isOddM yr == (s_ == s)
            return (r, s, isOddY, isSecondKey)
  where
    halfN = fn $ nC `quot` 2

hashMsg :: forall h . Hash h => Coercible h BS.ShortByteString => B.ByteString -> Fn
hashMsg msg
    | BS.length h < 32 = error "digest of hash function is too short"
    | otherwise = shortBytesToFn (BS.take 32 h)
  where
    h = coerce (hashByteString_ @h msg)

-- -------------------------------------------------------------------------- --
-- ECDSA Properties

prop_verify :: Int -> Property
prop_verify msg = ioProperty $ do
    (sk, pk) <- genKey
    (r, s, isOddY, isSecondKey) <- sign sk msgDigest
    return
        $ classify isOddY "isOddY"
        $ classify isSecondKey "isSecondKey"
        $ verify msgDigest r s pk === Right True
  where
    msgDigest = hashMsg @Sha3_256 $ B8.pack $ show msg

prop_recover :: Int -> Property
prop_recover msg = ioProperty $ do
    (sk, pk) <- genKey
    (r, s, isOddY, isSecondKey) <- sign sk msgDigest
    return
        $ classify isOddY "isOddY"
        $ classify isSecondKey "isSecondKey"
        $ counterexample ("sk: " <> show sk)
        $ counterexample ("msgDigest: " <> show msgDigest)
        $ counterexample ("isOddY: " <> show isOddY)
        $ counterexample ("isSecondKey: " <> show isSecondKey)
        $ counterexample ("r: " <> show r)
        $ counterexample ("s: " <> show s)
        $ recoverPublicKey msgDigest r s isOddY isSecondKey === Just pk
  where
    msgDigest = hashMsg @Sha3_256 $ B8.pack $ show msg

properties_ecdsa :: TestTree
properties_ecdsa = testGroup "ECDSA"
    [ testProperty "prop_verify" prop_verify
    , testProperty "prop_recover" prop_recover
    ]

-- -------------------------------------------------------------------------- --
-- Public ECDSA API

ecdsaGenKey :: IO (Fn, EcdsaPublicKey)
ecdsaGenKey = do
    k <- genKey
    traverse (ecdsaPublicKey . pointToBytes) k
  where
    pointToBytes :: Point -> BS.ShortByteString
    pointToBytes (Point x y) = BS.cons 0x04 (fpToShortBytes x <> fpToShortBytes y)
    pointToBytes O = BS.pack [0x00]

ecdsaSign_
    :: Word8
    -> Fn
    -> B.ByteString
    -> IO (EcdsaR, EcdsaS, EcdsaV)
ecdsaSign_ x sk msg = do
    (r, s, isOddY, isSecondKey) <- sign sk (hashMsg @Sha3_256 msg)
    (,,)
        <$> ecdsaR (fnToShortBytes r)
        <*> ecdsaS (fnToShortBytes s)
        <*> ecdsaV (BS.singleton (x + if isOddY then 1 else 0 + if isSecondKey then 1 else 0))

ecdsaSignEip155
    :: Fn
    -> B.ByteString
    -> IO (EcdsaR, EcdsaS, EcdsaV)
ecdsaSignEip155 = ecdsaSign_ 37

ecdsaSignOrig
    :: Fn
    -> B.ByteString
    -> IO (EcdsaR, EcdsaS, EcdsaV)
ecdsaSignOrig = ecdsaSign_ 27

ecdsaHashMsg
    :: forall h
    . Hash h
    => Coercible h BS.ShortByteString
    => B.ByteString
    -> EcdsaMessageDigest
ecdsaHashMsg msg = case ecdsaMessageDigest h of
    Left e -> error (show e)
    Right d -> d
  where
    h = coerce (hashByteString_ @h msg)

prop_ecdsa_verify :: Int -> Property
prop_ecdsa_verify msg = ioProperty $ do
    (sk, pk) <- ecdsaGenKey
    (r, s, _v) <- ecdsaSignEip155 sk msgBytes
    return $ case ecdsaVerify msgDigest pk r s of
        Right x -> x === True
        Left e -> counterexample (show e) $ False
  where
    msgBytes = B8.pack $ show msg
    msgDigest = ecdsaHashMsg @Sha3_256 msgBytes

prop_ecdsa_recover_orig :: Int -> Property
prop_ecdsa_recover_orig msg = ioProperty $ do
    (sk, pk) <- ecdsaGenKey
    (r, s, v) <- ecdsaSignOrig sk msgBytes
    return
        $ counterexample ("sk: " <> show sk)
        $ counterexample ("msgDigest: " <> show msgDigest)
        $ counterexample ("r: " <> show r)
        $ counterexample ("s: " <> show s)
        $ counterexample ("v: " <> show v)
        $ ecdsaRecoverPublicKey msgDigest r s v === Just pk
  where
    msgBytes = B8.pack $ show msg
    msgDigest = ecdsaHashMsg @Sha3_256 msgBytes

prop_ecdsa_recover_eip155 :: Int -> Property
prop_ecdsa_recover_eip155 msg = ioProperty $ do
    (sk, pk) <- ecdsaGenKey
    (r, s, v) <- ecdsaSignEip155 sk msgBytes
    return
        $ counterexample ("sk: " <> show sk)
        $ counterexample ("msgDigest: " <> show msgDigest)
        $ counterexample ("r: " <> show r)
        $ counterexample ("s: " <> show s)
        $ counterexample ("v: " <> show v)
        $ ecdsaRecoverPublicKey msgDigest r s v === Just pk
  where
    msgBytes = B8.pack $ show msg
    msgDigest = ecdsaHashMsg @Sha3_256 msgBytes

properties_ecdsa_api :: TestTree
properties_ecdsa_api = testGroup "ECDSA"
    [ testProperty "prop_ecdsa_verify" prop_ecdsa_verify
    , testProperty "prop_ecdsa_recover_orig" prop_ecdsa_recover_orig
    , testProperty "prop_ecdsa_recover_eip155" prop_ecdsa_recover_eip155
    ]

-- -------------------------------------------------------------------------- --
-- Prime Field

instance KnownNat n => Arbitrary (Zm n) where
    arbitrary = zm <$> arbitrary

-- Properties of Addition

prop_Zm_add_assoc :: forall n . KnownNat n => Zm n -> Zm n -> Zm n -> Property
prop_Zm_add_assoc a b c = (a .+ b) .+ c === a .+ (b .+ c)

prop_Zm_add_comm :: forall n . KnownNat n => Zm n -> Zm n -> Property
prop_Zm_add_comm a b = (a .+ b) === (b .+ a)

prop_Zm_add_neutral_r :: forall n . KnownNat n => Zm n -> Property
prop_Zm_add_neutral_r a = a .+ zm 0 === a

prop_Zm_add_neutral_l :: forall n . KnownNat n => Zm n -> Property
prop_Zm_add_neutral_l a = zm 0 .+ a === a

prop_Zm_add_inverse_r :: forall n . KnownNat n => Zm n -> Property
prop_Zm_add_inverse_r a = a .+ minusM a === zm 0

prop_Zm_add_inverse_l :: forall n . KnownNat n => Zm n -> Property
prop_Zm_add_inverse_l a = minusM a .+ a === zm 0

prop_Zm_add_dual_r :: forall n . KnownNat n => Zm n -> Zm n -> Property
prop_Zm_add_dual_r a b = (a .- b) .+ b === a

prop_Zm_add_dual_l :: forall n . KnownNat n => Zm n -> Zm n -> Property
prop_Zm_add_dual_l a b = b .+ (a .- b) === a

-- Properties of Multiplication

prop_Zm_mul_assoc :: forall n . KnownNat n => Zm n -> Zm n -> Zm n -> Property
prop_Zm_mul_assoc a b c = (a .* b) .* c === a .* (b .* c)

prop_Zm_mul_comm :: forall n . KnownNat n => Zm n -> Zm n -> Property
prop_Zm_mul_comm a b = (a .* b) === b .* a

prop_Zm_mul_neutral_r :: forall n . KnownNat n => Zm n -> Property
prop_Zm_mul_neutral_r a = a .* zm 1 === a

prop_Zm_mul_neutral_l :: forall n . KnownNat n => Zm n -> Property
prop_Zm_mul_neutral_l a = zm 1 .* a === a

-- This is a bit tricky because @f 0 ./ f 0 === f 0@ due to short cut semantics
-- in the definition of multiplication. We may want to change that.
--
prop_Zm_mul_inverse_r :: forall n . KnownNat n => Zm n -> Property
prop_Zm_mul_inverse_r a = a /= zm 0 ==> a .* invM a === zm 1

prop_Zm_mul_inverse_l :: forall n . KnownNat n => Zm n -> Property
prop_Zm_mul_inverse_l a = a /= zm 0 ==> invM a .* a === zm 1

prop_Zm_mul_dual_r :: forall n . KnownNat n => Zm n -> Zm n -> Property
prop_Zm_mul_dual_r a b = b > zm 0 ==> (a ./ b) .* b === a

prop_Zm_mul_dual_l :: forall n . KnownNat n => Zm n -> Zm n -> Property
prop_Zm_mul_dual_l a b = b > zm 0 ==> b .* (a ./ b) === a

properties_Zm :: forall n . KnownNat n => String -> TestTree
properties_Zm l = testGroup ("Zm " <> l <> " (" <> show (natVal_ @n) <> ")")
    -- additive properties
    [ testProperty "prop_Zm_add_assoc" (prop_Zm_add_assoc @n)
    , testProperty "prop_Zm_add_comm" (prop_Zm_add_comm @n)
    , testProperty "prop_Zm_add_neutral_r" (prop_Zm_add_neutral_r @n)
    , testProperty "prop_Zm_add_neutral_l" (prop_Zm_add_neutral_l @n)
    , testProperty "prop_Zm_add_inverse_r" (prop_Zm_add_inverse_r @n)
    , testProperty "prop_Zm_add_inverse_l" (prop_Zm_add_inverse_l @n)
    , testProperty "prop_Zm_add_dual_r" (prop_Zm_add_dual_r @n)
    , testProperty "prop_Zm_add_dual_l" (prop_Zm_add_dual_l @n)

    -- multiplicative properties
    , testProperty "prop_Zm_mul_assoc" (prop_Zm_mul_assoc @n)
    , testProperty "prop_Zm_mul_comm" (prop_Zm_mul_comm @n)
    , testProperty "prop_Zm_mul_neutral_r" (prop_Zm_mul_neutral_r @n)
    , testProperty "prop_Zm_mul_neutral_l" (prop_Zm_mul_neutral_l @n)
    , testProperty "prop_Zm_mul_inverse_r" (prop_Zm_mul_inverse_r @n)
    , testProperty "prop_Zm_mul_inverse_l" (prop_Zm_mul_inverse_l @n)
    , testProperty "prop_Zm_mul_dual_r" (prop_Zm_mul_dual_r @n)
    , testProperty "prop_Zm_mul_dual_l" (prop_Zm_mul_dual_l @n)
    ]

properties_Fp :: TestTree
properties_Fp = properties_Zm @PC "p"

properties_Fn :: TestTree
properties_Fn = properties_Zm @NC "n"

-- -------------------------------------------------------------------------- --
-- Prime field

-- Square and Square Root

prop_F_sqr :: Fp -> Property
prop_F_sqr a = a .^ 2 === minusM a .^ 2

prop_F_sqrt0 :: Fp -> Property
prop_F_sqrt0 a = sqrtFp a === sqrtFp2 a .&&. sqrtFp a === sqrtFp3 a

prop_F_sqrt1 :: Fp -> Property
prop_F_sqrt1 a = case sqrtFp a of
    (True, b) -> b .^ 2 === a
    (False, b) -> b .^ 2 === minusM a

properties_Fp_sqrt :: TestTree
properties_Fp_sqrt = testGroup "Fp sqrt"
    [ testProperty "prop_F_sqr" prop_F_sqr
    , testProperty "prop_F_sqrt0" prop_F_sqrt0
    , testProperty "prop_F_sqrt1" prop_F_sqrt1
    ]

-- -------------------------------------------------------------------------- --
-- Curve Points

instance Arbitrary Point where
    arbitrary = frequency
        [ (1, pure O)
        , (9, point <$> arbitrary)
        ]
        -- TODO use a generator that uses pointFromX?

prop_P_add_assoc :: Point -> Point -> Point -> Property
prop_P_add_assoc a b c = (a .+. b) .+. c === a .+. (b .+. c)

prop_P_add_comm :: Point -> Point -> Property
prop_P_add_comm a b = a .+. b === b .+. a

prop_P_add_neutral_r :: Point -> Property
prop_P_add_neutral_r a = a .+. O === a

prop_P_add_neutral_l :: Point -> Property
prop_P_add_neutral_l a = O .+. a === a

prop_P_add_inverse_r :: Point -> Property
prop_P_add_inverse_r a = a .+. minusP a === O

prop_P_add_inverse_l :: Point -> Property
prop_P_add_inverse_l a = minusP a .+. a === O

prop_P_add_inverse_2 :: Point -> Point -> Property
prop_P_add_inverse_2 a b = a .-. b === minusP (b .-. a)

prop_P_mul :: Fp -> Point -> Property
prop_P_mul n p = montgomeryMult n p === doubleAndAdd (nat n) p

prop_key_compression :: Fn -> Property
prop_key_compression sk = validatePublicKey pk ==>
    (Just pk === pointFromX pkx True .||. Just pk === pointFromX pkx False)
  where
    pk = sk .*. gC
    pkx = case pk of
        (Point x _) -> x
        O -> error "invalid public key, point at infinity"

properties_P :: TestTree
properties_P = testGroup "Point"
    [ testProperty "prop_P_add_assoc" prop_P_add_assoc
    , testProperty "prop_P_add_comm" prop_P_add_comm
    , testProperty "prop_P_add_neutral_l" prop_P_add_neutral_l
    , testProperty "prop_P_add_neutral_r" prop_P_add_neutral_r
    , testProperty "prop_P_add_inverse_r" prop_P_add_inverse_r
    , testProperty "prop_P_add_inverse_l" prop_P_add_inverse_l
    , testProperty "prop_P_add_inverse_2" prop_P_add_inverse_2
    , testProperty "prop_P_mul" prop_P_mul
    , testProperty "prop_key_compression" prop_key_compression
    ]

-- -------------------------------------------------------------------------- --
-- All Properties

tests :: TestTree
tests = testGroup "Crypto.Secp256k1"
    [ properties_Fp
    , properties_Fn
    , properties_Fp_sqrt
    , properties_P
    , properties_ecdsa
    , properties_ecdsa_api
    , properties_example1
    ]
