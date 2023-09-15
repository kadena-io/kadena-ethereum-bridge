{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: Crypto.Secp256k1.Internal
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Low-Level implementation of Secp256k1 ECDSA signature verification with
-- public key recovery.
--
-- Implementation is based on http://www.secg.org/sec1-v2.pdf
--
-- Only operations that do not involve private key material are implemented.
--
module Crypto.Secp256k1.Internal
(
--  * Modulo Arithmethic
  Zm
, pattern Zm
, zm
, nat
, byteLength
, bitLength
, (.+)
, (.-)
, (.*)
, (./)
, (.^)
, minusM
, invM
, isOddM
, zConv
, bytesToZm
, shortBytesToZm
, zmToBytes
, zmToShortBytes

-- * Prime Field for curve Secp256k1

, type PC
, Fp
, pattern Fp
, fp
, bytesToFp
, shortBytesToFp
, fpToBytes
, fpToShortBytes
, sqrtFp
, sqrtFp1
, sqrtFp2
, sqrtFp3

-- * Order of curve Secp256k1

, type NC
, Fn
, pattern Fn
, fn
, bytesToFn
, shortBytesToFn
, fnToBytes
, fnToShortBytes

-- * Elliptic curve points for Secp256k1

, Point(O)
, pattern Point

-- ** Curve Secp256k1
, pC
, nC
, gC
, hC
, aC
, bC

-- * Arithmetic for Weierstrass curves
, (.+.)
, (.-.)
, (.*.)
, (*.)
, minusP
, montgomeryMult
, doubleAndAdd
, pointFromX
, isOnCurve
, maybePoint
, point

-- * ECDSA for Secp256k1
, verify
, recoverPublicKey
, validatePublicKey
, validateSecretKey
, getPublicKey
, maybePublicKey

-- ** Printing
, zm2hex
, zm2hex_
, hex2zm
, hex2zm_
, p2hex

-- ** Miscelaneous
, int
, natVal_
, intVal_
, sshow
, bytesToNat
, natToBytes
, shortBytesToNat
) where

import Control.Monad

import Data.Bits (Bits, shiftL, testBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Word

import GHC.Natural (powModNatural, Natural)
import GHC.Stack
import qualified GHC.TypeLits as I (KnownNat, natVal')
import GHC.TypeNats (KnownNat, natVal')
#if ! MIN_VERSION_base(4,16,0)
import GHC.TypeNats (Nat)
#endif

import Text.Printf

#if MIN_VERSION_base(4,15,0)
import GHC.Num (integerRecipMod#, integerLog2)
#else
import GHC.Exts (Int(..))
import GHC.Integer.GMP.Internals (recipModInteger)
import GHC.Integer.Logarithms (integerLog2#)
#endif

import GHC.Exts (Proxy#, proxy#)

-- -------------------------------------------------------------------------- --
-- Operators

infixl 6 .+, .-, .+., .-.
infixl 7 .*, ./, .*., *.
infixr 8 .^

-- -------------------------------------------------------------------------- --
-- Utils

#if MIN_VERSION_base(4,16,0)
type Nat = Natural
#endif

#if ! MIN_VERSION_base(4,15,0)
integerLog2 :: Integer -> Int
integerLog2 x = case integerLog2# x of a -> I# a
#endif

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

natVal_ :: forall n . KnownNat n => Natural
natVal_ = natVal' (proxy# :: Proxy# n)
{-# INLINE natVal_ #-}

intVal_ :: forall n . I.KnownNat n => Integer
intVal_ = I.natVal' (proxy# :: Proxy# n)
{-# INLINE intVal_ #-}

bitLength_ :: Natural -> Int
bitLength_ 0 = 0
bitLength_ n = int $ integerLog2 (int  $ n - 1) + 1

bitLength :: forall n . KnownNat n => Int
bitLength = bitLength_ (natVal_ @n)

byteLength_ :: Natural -> Int
byteLength_ n = (bitLength_ n - 1) `div` 8 + 1

byteLength :: forall n . KnownNat n => Int
byteLength = byteLength_ (natVal_ @n)

sshow :: Show a => IsString b => a -> b
sshow = fromString . show
{-# INLINE sshow #-}

-- Big Endian Encodings:
--
-- The implementation is not optimized

shortBytesToNat :: BS.ShortByteString -> Natural
shortBytesToNat = BS.foldl' (\a b -> shiftL a 8 + int b) 0

bytesToNat :: B.ByteString -> Natural
bytesToNat = B.foldl' (\a b -> shiftL a 8 + int b) 0

-- | The first parameter determines the length in bytes of the result
-- The result is either padded with zero bytes or truncated.
--
natToShortBytes
    :: Int
    -> Natural
    -> BS.ShortByteString
natToShortBytes l = BS.pack . natToBytesInternal l


-- | The first parameter determines the length in bytes of the result
-- The result is either padded with zero bytes or truncated.
--
natToBytes
    :: Int
    -> Natural
    -> B.ByteString
natToBytes l = B.pack . natToBytesInternal l

natToBytesInternal
    :: Int
    -> Natural
    -> [Word8]
natToBytesInternal l n = go l n []
  where
    go 0 _ = id
    go !i m = let (a, b) = quotRem m 256 in go (i - 1) a . (int b :)

-- -------------------------------------------------------------------------- --
-- Arithmetic modulo n

-- | Modulo Rings
--
-- The constructor should not be used directly.
--
-- For constructing values use one of the provided smart constructors. For
-- pattern use one of the provided pattern synonyms or the @nat@ function.
--
--
newtype M (n :: Nat) = M Natural
    deriving (Eq, Ord)
    deriving newtype (Bits)

instance KnownNat n => Show (M n) where
    show = zm2hex

type Zm = M
pattern Zm :: Natural -> M n
pattern Zm n <- M n
{-# COMPLETE Zm #-}

zm :: forall n . KnownNat n => Natural -> M n
zm a = M $! a `rem` (natVal_ @n)

-- | Convert an element of a modulo ring to a natural number
--
nat :: M n -> Natural
nat (M a) = a

bytesToZm :: forall n . KnownNat n => B.ByteString -> Zm n
bytesToZm = zm . bytesToNat . B.take (byteLength @n)

shortBytesToZm :: forall n . KnownNat n => BS.ShortByteString -> Zm n
shortBytesToZm = zm . shortBytesToNat . BS.take (byteLength @n)

zmToBytes :: forall n . KnownNat n => Zm n -> B.ByteString
zmToBytes = natToBytes (byteLength @n) . nat

zmToShortBytes :: forall n . KnownNat n => Zm n -> BS.ShortByteString
zmToShortBytes = natToShortBytes (byteLength @n) . nat

(.+) :: KnownNat n => M n -> M n -> M n
(M a) .+ (M b) = zm (a + b)

minusM :: forall n . KnownNat n => M n -> M n
minusM (M 0) = M 0
minusM (M a) = M (natVal_ @n - a)

(.-) :: KnownNat n => M n -> M n -> M n
a .- b = a .+ minusM b

(.*) :: KnownNat n => M n -> M n -> M n
(M a) .* (M b) = zm (a * b)

invM :: forall n . HasCallStack => KnownNat n => M n -> M n
invM (M 0) = M $! div 0 0
#if MIN_VERSION_base(4,15,0)
invM (M a) = M $! case integerRecipMod# (int a) (natVal_ @n) of
    (# n | #)-> n
    (# | () #) -> error "integerRecipMod#: no result"
#else
invM (M a) = M $! int (recipModInteger (int a) (intVal_ @n))
#endif

(./) :: HasCallStack => KnownNat n => M n -> M n -> M n
_ ./ (M 0) = M $! div 0 0
a ./ b = a .* invM b

(.^) :: forall n . KnownNat n => M n -> Natural -> M n
(.^) (M a) n = M $! powModNatural a n (natVal_ @n)

zConv :: KnownNat m => Zm n -> Zm m
zConv (M n) = zm n

isOddM :: M n -> Bool
isOddM (M a) = a `rem` 2 == 1

-- -------------------------------------------------------------------------- --
-- | Prime of curve Secp256k1
--
type PC :: Nat
type PC = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | Prime Field for Curve Secp256k1
--
type Fp = M PC

pattern Fp :: Natural -> Fp
pattern Fp n <- M n
{-# COMPLETE Fp #-}

fp :: Natural -> Fp
fp = zm

bytesToFp :: B.ByteString -> Fp
bytesToFp = bytesToZm

shortBytesToFp :: BS.ShortByteString -> Fp
shortBytesToFp = fp . shortBytesToNat . BS.take 32

fpToBytes :: Fp -> B.ByteString
fpToBytes = zmToBytes

fpToShortBytes :: Fp -> BS.ShortByteString
fpToShortBytes = zmToShortBytes

-- -------------------------------------------------------------------------- --
-- | Order of curve Secp256k1
--
type NC :: Nat
type NC = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

-- | Modulo arithmetic over the order of curve Secp256k1
--
type Fn = M NC

pattern Fn :: Natural -> Fn
pattern Fn n <- M n
{-# COMPLETE Fn #-}

fn :: Natural -> Fn
fn = zm

bytesToFn :: B.ByteString -> Fn
bytesToFn = bytesToZm

shortBytesToFn :: BS.ShortByteString -> Fn
shortBytesToFn = fn . shortBytesToNat . BS.take 32

fnToBytes :: Fn -> B.ByteString
fnToBytes = zmToBytes

fnToShortBytes :: Fn -> BS.ShortByteString
fnToShortBytes = zmToShortBytes

-- -------------------------------------------------------------------------- --
-- Solving Quadratic Resdiue in in  F_p

-- Benchmarks indicate that sqrt1 and sqrt2 perform similar. Sqrt3 is slightly
-- slower.

-- | Quadratic residue
--
sqrtFp :: Fp -> (Bool, Fp)
sqrtFp = sqrtFp1

sqrtFp1 :: Fp -> (Bool, Fp)
sqrtFp1 a = (r .^ 2 == a, r)
  where
    r = a .^ ((pC + 1) `quot` 4)

--
sqrtFp2 :: Fp -> (Bool, Fp)
sqrtFp2 a = (r .^ 2 == a, r)
  where
    r = (a .^ ((pC + 1) `quot` 8)) .^ 2

-- | Source of the follwoing algorith is th scp256k1 C library.
--
sqrtFp3 :: Fp -> (Bool, Fp)
sqrtFp3 a = (r .^ 2 == a, r)
  where
    x2 = a .^ p2 1 .* a
    x3 = x2 .^ p2 1 .* a
    x6 = x3 .^ p2 3 .* x3
    x9 = x6 .^ p2 3 .* x3
    x11 = x9 .^ p2 2 .* x2
    x22 = x11 .^ p2 11 .* x11
    x44 = x22 .^ p2 22 .* x22
    x88 = x44 .^ p2 44 .* x44
    x176 = x88 .^ p2 88 .* x88
    x220 = x176 .^ p2 44 .* x44
    x223 = x220 .^ p2 3 .* x3
    t1 = ((x223 .^ p2 23 .* x22) .^ p2 6 .* x2) .^ 2
    r = t1 .^ 2

    p2 :: Int -> Natural
    p2 = (2 ^)

-- -------------------------------------------------------------------------- --
-- Points on the Curve Secp256k1

-- | Eliptic Curve Points
--
data Point = O | P !Fp !Fp
    deriving (Show, Eq, Ord)

-- | Accessor for the coordicates of a Point
--
-- @O@ is exported as constructor
--
pattern Point :: Fp -> Fp -> Point
pattern Point x y <- P x y
{-# COMPLETE O, Point #-}

-- -------------------------------------------------------------------------- --
-- Secp256k1 Curve Parameters
--
-- \(y^2 = x^3 + aC * x + bC \) over the finite prime Field \(F_{pC}\)
--
-- with parameters \( (pC,aC,bC,GC,nC,hC) \) defined as follows.
--
-- Note that \(a\) is zero.
--
-- cf. https://www.secg.org/sec2-v2.pdf, 2.4.1
--

-- | Characteristic @p@
--
-- \( 2^{256} - 2^{32} - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1 \)
--
pC :: Natural
pC = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

-- | Coefficient @a@
--
aC :: Fp
aC = fp 0x0

-- | Coefficient @b@
--
bC :: Fp
bC = fp 0x7

-- | x coordinate @G_x@ of base point @G@
--
gCx :: Fp
gCx = fp 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798

-- | y coordinate @G_y@ of base pont @G@
--
gCy :: Fp
gCy = fp 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8

-- | Base point @G@
--
gC :: Point
gC = P gCx gCy

-- | Order @n@.
--
-- Because @h = order(Curve)/order(G) = 1@ it is both the order of the curve and
-- of the base point @G@.
--
nC :: Natural
nC = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

-- | Cofactor @h@. Because this is 1, the order of the curve and the base point
-- are the same. I.e. @aG@, for @a=0,1,...@ covers all points on the curve.
--
hC :: Natural
hC = 0x1

-- -------------------------------------------------------------------------- --
-- Arithmetic on Weirstrass Curves

-- | Point Addition
--
(.+.) :: Point -> Point -> Point
(.+.) a O = a
(.+.) O a = a

-- distinct points (which are not additive inverse of each other)
--
(.+.) (P x1 y1) (P x2 y2) | x1 /= x2 = P x3 y3
  where
    l = (y2 .- y1) ./ (x2 .- x1)
    x3 = ((l .^ 2) .- x1) .- x2
    y3 = (l .* (x1 .- x3)) .- y1

-- doubling
--
(.+.) (P x1 y1) (P _x2 y2) | y1 == y2 && y1 /= M 0 = P x3 y3
  where
    l = ((M 3 .* (x1 .^ 2)) .+ aC) ./ (M 2 .* y1)
    x3 = (l .^ 2) .- (M 2 .* x1)
    y3 = (l .* (x1 .- x3)) .- y1

-- inverse points (y1 == -y2)
--
(.+.) _ _ = O

minusP :: Point -> Point
minusP O = O
minusP (P ax ay) = P ax (minusM ay)

(.-.) :: Point -> Point -> Point
a .-. b = a .+. minusP b

-- | Point multiplication by field element.
--
(.*.) :: Fn -> Point -> Point
(.*.) = montgomeryMult

-- montgomeryMult is only  slightly slower than doubleAndAdd.

-- | Point multiplication by integral element.
--
(*.) :: Natural -> Point -> Point
(*.) = doubleAndAdd

-- | Compute curve point from a scalar by multiplying with the generator.
-- This covers the whole curve for curves with cofactor of 1.
--
point :: Fn -> Point
point x = x .*. gC

maybePoint :: Fp -> Fp -> Maybe Point
maybePoint x y = if isOnCurve p then Just p else Nothing
  where
    p = P x y

isOnCurve :: Point -> Bool
isOnCurve O = True
isOnCurve (P x y) = x .^ 3 .+ x .* aC .+ bC == y .^ 2

doubleAndAdd :: Natural -> Point -> Point
doubleAndAdd n p
    | n < 0 = minusP $ (-n) *. p
    | n == 0 = O
    | n == 1 = p
    | even n = p'
    | otherwise = p .+. p'
  where
    p' = doubleAndAdd (div n 2) (p .+. p)

-- | Montomery Ladder for scalar multiplication of curve points
--
-- Less vulnerable against timing attacks
--
montgomeryMult :: forall n . KnownNat n => Zm n -> Point -> Point
montgomeryMult n = go (bitLength @n - 1) O
  where
    go (-1) r0 _ = r0
    go m r0 r1
        | testBit n m = go (m - 1) (r0 .+. r1) (r1 .+. r1)
        | otherwise = go (m - 1) (r0 .+. r0) (r0 .+. r1)

-- -------------------------------------------------------------------------- --
-- Public Keys for Secp256k1

-- | It is assumed that the caller guarantees that x and y are elements of F.
--
validatePublicKey :: Point -> Bool
validatePublicKey O = False
validatePublicKey pk = checkOrder && isOnCurve pk
  where
    checkOrder = nC *. pk == O -- Is this needed for secp256k1?

validateSecretKey :: Fn -> Bool
validateSecretKey (Fn a) = 0 < a && a < nC

getPublicKey :: Fn -> Point
getPublicKey sk = sk .*. gC

maybePublicKey :: Fp -> Fp -> Maybe Point
maybePublicKey x y = if validatePublicKey p then Just p else Nothing
  where
    p = P x y

-- | Point decompression for curve Secp256k1
--
pointFromX :: Fp -> Bool -> Maybe Point
pointFromX x oddY = P x <$> case sqrtFp ys of
    (True, y) -> Just $ if oddY == isOddM y then y else minusM y
    (False, _) -> Nothing -- invalid point
  where
    ys = x .^ 3 .+ aC .* x .+ bC
    -- equivalent: ys = x .^ 3 .+ bC for secp256k1

-- -------------------------------------------------------------------------- --
-- Secp256k1 ECDSA Signatur verification

-- | Lowlevel ECDSA signature validatio for curve Secp256k1.
--
verify
    :: Fn
        -- ^ Message Digest
    -> Fn
        -- ^ r
    -> Fn
        -- ^ s
    -> Point
        -- ^ public key
    -> Either T.Text Bool
verify e r s pk
    | not (validatePublicKey pk) = Left "invalid public key"
    | otherwise = case rP of
        -- O -> Right False
        O -> Left "got point at infinity during signature verification"
        P (Zm xr) _ -> Right $ zm xr == r
  where
    u1 = e .* is
    u2 = r .* is
    rP = u1 .*. gC .+. u2 .*. pk -- TODO use Shamir's algorithm to make this more efficient
    is = invM s

-- | Recover public key for curve Secp256k1.
--
-- The chance that `secondKey` is needed (i.e. there are two solution for the x
-- coordinate) is one in \(2^{128}\). Do we need to cover that case or can we just
-- ignore it? Could ignoring it lead to attacks on the chain? We don't know how
-- to test that case.
--
-- The arithmethic in this function can be optimized. For instance cf.
-- https://github.com/indutny/elliptic/blob/43ac7f230069bd1575e1e4a58394a512303ba803/lib/elliptic/ec/index.js#L196
--
recoverPublicKey
    :: Fn
        -- ^ Message Digest
    -> Fn
        -- ^ r
    -> Fn
        -- ^ s
    -> Bool
        -- ^ odd Y
    -> Bool
        -- ^ is second key
    -> Maybe Point
recoverPublicKey e r s oddY secondKey = case nC *. rP of
    O -> if validatePublicKey pk then Just pk else Nothing
    _ -> error "something went wrong (probably the value for second key is incorrect)"
  where
    x = if secondKey then zConv r .+ fp nC else zConv r
    Just rP = pointFromX x oddY -- FIXME this fails for an invalid @secondKey@ value
    pk = invM r .*. (s .*. rP .-. e .*. gC)

-- -------------------------------------------------------------------------- --
-- Hexdecimal Representation

-- | In the contex of Ethereum points are always stored uncompressed, which
-- is indicated by prefixing them with 0x4.
--
-- uncrompressedPointPrefix :: Word8
-- uncrompressedPointPrefix = 0x4

zm2hex_ :: forall n s . KnownNat n => IsString s => Zm n -> s
zm2hex_ (Zm n) = fromString $ printf @(Natural -> String) format n
  where
    format = "%0" <> show (byteLength @n * 2) <> "x"

zm2hex :: forall n s . KnownNat n => IsString s => Zm n -> s
zm2hex (Zm n) = fromString $ printf @(Natural -> String) format n
  where
    format = "0x%0" <> show (byteLength @n * 2) <> "x"

hex2zm_ :: forall n . KnownNat n => T.Text -> Either T.Text (Zm n)
hex2zm_ t
    | T.length t == byteLength @n * 2 = case T.hexadecimal t of
        Right (x, "") -> Right $ zm x
        Right (x, _) -> Left $ "pending characters after parsing " <> sshow x
        Left e -> Left (T.pack e)
    | otherwise = Left $ "Wrong input length: expected " <> sshow (byteLength @n * 2) <> ", got " <> sshow (T.length t)

hex2zm :: forall n . KnownNat n => T.Text -> Either T.Text (Zm n)
hex2zm = strip0x >=> hex2zm_

strip0x :: T.Text -> Either T.Text T.Text
strip0x t = case T.stripPrefix "0x" t of
    Just x -> Right x
    Nothing -> Left $ "Missing hex prefix 0x in " <> t
{-# INLINE strip0x #-}

p2hex :: IsString s => Point -> s
p2hex O = "0x00"
p2hex (P x y) = fromString $ "0x4" <> zm2hex_ x <> zm2hex_ y

