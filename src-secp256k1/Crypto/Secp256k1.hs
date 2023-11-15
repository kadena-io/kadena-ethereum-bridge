{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Crypto.Secp256k1
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Crypto.Secp256k1
( EcdsaPublicKey
, ecdsaPublicKey
, ecdsaPublicKeyBytes
, EcdsaMessageDigest
, ecdsaMessageDigest
, EcdsaR
, ecdsaR
, EcdsaS
, ecdsaS
, EcdsaV
, ecdsaV
, ecdsaVerify
, ecdsaRecoverPublicKey
) where

import Control.Monad
import Control.Monad.Catch

import qualified Data.ByteString.Short as BS
import qualified Data.Text as T

import Numeric.Natural

-- internal modules

import Crypto.Secp256k1.Internal

-- -------------------------------------------------------------------------- --
-- Exceptions

newtype EcdsaException = EcdsaException T.Text
    deriving (Show, Eq, Ord)

instance Exception EcdsaException

-- -------------------------------------------------------------------------- --
-- Utils

checkLength :: MonadThrow m => T.Text -> Natural -> BS.ShortByteString -> m BS.ShortByteString
checkLength label n bs
    | l > n = throwM $ EcdsaException $
        label <> ".checkLength: input too long. Expected " <> sshow n <> " but got " <> sshow l <> " bytes."
    | l < n = throwM $ EcdsaException $
        label <> ".checkLength: input too short. Expected " <> sshow n <> " but got " <> sshow l <> " bytes."
    | otherwise = return bs
  where
    l = int $ BS.length bs

pointToBytes :: Point -> BS.ShortByteString
pointToBytes (Point x y) = BS.cons 0x04 (fpToShortBytes x <> fpToShortBytes y)
pointToBytes O = BS.pack [0x00]

publicKeyPointFromBytes :: MonadThrow m => BS.ShortByteString -> m Point
publicKeyPointFromBytes (BS.unpack -> [0x00]) = throwM $ EcdsaException
    "pointFromBytes: point of infinity can't be used as public key"
publicKeyPointFromBytes bs = do
    void $ checkLength "pointFrombBytes" 65 bs
    (x, y) <- case BS.uncons bs of
        Just (0x04, r) -> return $ BS.splitAt 32 r
        Just (x, _) -> throwM $ EcdsaException $
            "pointFromBytes: unsupported encoding. Expected 0x04 (uncomporessed point), but got " <> sshow x
        Nothing -> error "pointFromBytes: missing first byte of input"
            -- can't happend because we check that input is of length 65

    case maybePublicKey (shortBytesToFp x) (shortBytesToFp y) of
        Nothing -> throwM $ EcdsaException
            "pointFromBytes: invalid public key. Coordinates are not a point on the curve"
        Just p -> return p

-- -------------------------------------------------------------------------- --
-- Public API

newtype EcdsaPublicKey = EcdsaPublicKey Point
    deriving (Show, Eq)

newtype EcdsaMessageDigest = EcdsaMessageDigest Fn
    deriving (Show, Eq)

newtype EcdsaR = EcdsaR Fn
    deriving (Show, Eq)

newtype EcdsaS = EcdsaS Fn
    deriving (Show, Eq)

-- | The recovery id or V value of an ECDSA signature is used for public key
-- recovery. If present it indicates the parity of the y-coordinate of the
-- public key and whether the magnitude of the x-coordinate is lower than the curve order.
--
-- In Ethereum a value is originally computed as
-- \(27 + y-parity + (magnitude of x is lower curve order)\).
--
-- Since EIP-155 the value is computed for the Ethereum mainnet as
-- \(37 + y-parity + (magnitude of x is lower curve order)\).
--
-- Mainy external tools still use the original values and implementations
-- should therefore support both options.
--
-- Note, that the values 29, 30, 39, and 40 are extremly unlikely to occur with
-- randomly generate keys.
--
-- Also note, that the V value may not always be provided with the signature
-- in which case it is safe to just try all possible values when recovering
-- the public key.
--
data EcdsaV = EcdsaV
    !Bool
        -- ^ whether parity of the y-coordinate of public recovered public key
        -- is odd.
        --
        -- If you don't know this parameter it is safe to try both options, at
        -- the cost of taking on average 1.5 times more computation time to
        -- compute the result.

    !Bool
        -- ^ whether the second solution for the public key is returned. This
        -- parameter is almost surely always @False@.
        --
        -- If you don't know this value it is safe to assume that it is @False@.
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
        -- case. Although, it is not clear whether creating such an attack is infeasible.
    deriving (Show, Eq)

-- | Input: 65 bytes that represent an uncompressed (prefix 0x04) secp256k1
-- curve point.
--
ecdsaPublicKey :: MonadThrow m => BS.ShortByteString -> m EcdsaPublicKey
ecdsaPublicKey = fmap EcdsaPublicKey . publicKeyPointFromBytes

-- | Returns 65 bytes that represent a public key encoded as uncompressed
-- secp256k1 curve point.
--
ecdsaPublicKeyBytes :: EcdsaPublicKey -> BS.ShortByteString
ecdsaPublicKeyBytes (EcdsaPublicKey p) = pointToBytes p

-- | Input: 32 bytes that represent a message digest.
--
-- It is expected that the digest is produced by a cryptographic hash functions
-- that produceds digests of at least 32 bytes. If the original digest has more
-- than 32 bytes the input should contain only the leftmost 32 bytes (assuming
-- that security of the hash is distributed uniformily accross all bits of the
-- original digest).
--
ecdsaMessageDigest :: MonadThrow m => BS.ShortByteString -> m EcdsaMessageDigest
ecdsaMessageDigest = fmap (EcdsaMessageDigest . shortBytesToFn)
    . checkLength "EcdsaMessageDigest" 32

-- | Input: 32 byte long R value of the secp256k1 ECDSA signature
--
ecdsaR :: MonadThrow m => BS.ShortByteString -> m EcdsaR
ecdsaR = fmap (EcdsaR . shortBytesToFn) . checkLength "ecdsaR" 32

-- | Input: 32 byte long S value of the secp256k1 ECDSA signature
--
ecdsaS :: MonadThrow m => BS.ShortByteString -> m EcdsaS
ecdsaS = fmap (EcdsaS . shortBytesToFn) . checkLength "ecdsaS" 32

-- | Input: 1 byte long V value of the secp256k1 ECDSA signature
--
ecdsaV :: MonadThrow m => BS.ShortByteString -> m EcdsaV
ecdsaV = checkLength "ecdsaV" 1 >=> \case

    -- before EIP-155
    "\27" -> return $ EcdsaV False False
    "\28" -> return $ EcdsaV True False
    "\29" -> return $ EcdsaV False True
    "\30" -> return $ EcdsaV True True

    -- EIP-155
    "\37" -> return $ EcdsaV False False
    "\38" -> return $ EcdsaV True False
    "\39" -> return $ EcdsaV False True
    "\40" -> return $ EcdsaV True True

    e -> throwM $ EcdsaException $
        "Invalid V value for signature: " <> sshow e

ecdsaVerify
    :: MonadThrow m
    => EcdsaMessageDigest
        -- ^ A 32 bytes long message digest that was compute with the same hash
        -- function that was used for producing the signature.
    -> EcdsaPublicKey
        -- ^ The public key of the signer encoded as 65 byte long uncompressed
        -- curve point.
    -> EcdsaR
        -- ^ The R value of the input signature
    -> EcdsaS
        -- ^ The S value of the input singature
    -> m Bool
ecdsaVerify (EcdsaMessageDigest d) (EcdsaPublicKey p) (EcdsaR r) (EcdsaS s) =
    case verify d r s p of
        Left t -> throwM $ EcdsaException t
        Right b -> return b

ecdsaRecoverPublicKey
    :: EcdsaMessageDigest
        -- ^ A 32 bytes long message digest that was compute with the same hash
        -- function that was used for producing the signature.
    -> EcdsaR
        -- ^ The R value of the input signature
    -> EcdsaS
        -- ^ The S value of the input singature
    -> EcdsaV
        -- ^ The recovery id or V value of the input singature
        --
        -- If you don't know this value you may just try all 4 possible value,
        -- where the values with 'ecdsaVHigh' are extremely unlikely.
        --
    -> Maybe EcdsaPublicKey
ecdsaRecoverPublicKey (EcdsaMessageDigest d) (EcdsaR r) (EcdsaS s) (EcdsaV isOddY isSecond) =
    EcdsaPublicKey <$> recoverPublicKey d r s isOddY isSecond

