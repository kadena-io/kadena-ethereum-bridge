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
newtype EcdsaMessageDigest = EcdsaMessageDigest Fn
newtype EcdsaR = EcdsaR Fn
newtype EcdsaS = EcdsaS Fn

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
    -> Bool
        -- ^ whether parity of the public recovered public key is odd.
        --
        -- If you don't know this parameter it is safe to try both options, at
        -- the cost of taking on average 1.5 times more computation time to
        -- compute the result.
    -> Bool
        -- ^ whether the second solution for the public key is returned. This
        -- parameter is almost surely always @False@.
        --
        -- If you don't know this value it is safe to assume that it is @False@.
        --
    -> Maybe EcdsaPublicKey
ecdsaRecoverPublicKey (EcdsaMessageDigest d) (EcdsaR r) (EcdsaS s) oddY secondKey =
    EcdsaPublicKey <$> recoverPublicKey d r s oddY secondKey

