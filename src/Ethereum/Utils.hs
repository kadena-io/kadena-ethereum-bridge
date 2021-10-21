{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Ethereum.Utils
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Ethereum.Utils
( int
, pow256
, natVal_
, intVal_
, symbolVal_

-- * Cryptography
, digestToShortByteString

-- * Encodings
, toByteString
, toShortByteString
, builderToText

-- * JSON
, HexQuantity(..)
, HexBytes(..)
, JsonCtx(..)

-- * Binary Encoding of Naturals
, encodeLe
, encodeBe

-- * base16-bytestring backward compat
, decode16
) where

import Control.Monad
#if ! MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

import Crypto.Hash (Digest)

import Data.Aeson
import Data.Aeson.Encoding hiding (int)
import Data.Aeson.Internal
import qualified Data.ByteArray as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.String
import Data.Word

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal', KnownNat, natVal')
import qualified GHC.TypeNats as N (KnownNat, natVal')

import Numeric.Natural

#if MIN_VERSION_bytestring(0,10,10)
import System.IO.Unsafe
#endif

import Text.Printf

import GHC.Exts (Proxy#, proxy#)

-- internal modules

import Numeric.Checked


-- -------------------------------------------------------------------------- --
-- Utils

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

pow256 :: Integral a => Int -> a
pow256 a = 256^a
{-# INLINE pow256 #-}

natVal_ :: forall n . N.KnownNat n => Natural
natVal_ = N.natVal' (proxy# :: Proxy# n)
{-# INLINE natVal_ #-}

intVal_ :: forall n . KnownNat n => Integer
intVal_ = natVal' (proxy# :: Proxy# n)
{-# INLINE intVal_ #-}

symbolVal_ :: forall n . KnownSymbol n => String
symbolVal_ = symbolVal' (proxy# :: Proxy# n)
{-# INLINE symbolVal_ #-}

-- -------------------------------------------------------------------------- --
-- JSON

str :: BB.Builder -> BB.Builder
str b = "\"" <> b <> "\""
{-# INLINE str #-}

toByteString :: BB.Builder -> B.ByteString
toByteString = BL.toStrict . BB.toLazyByteString
{-# INLINE toByteString #-}

toShortByteString :: BB.Builder -> BS.ShortByteString
toShortByteString = BS.toShort . BL.toStrict . BB.toLazyByteString
{-# INLINE toShortByteString #-}

builderToText :: BB.Builder -> T.Text
builderToText = T.decodeUtf8 . BL.toStrict . BB.toLazyByteString
{-# INLINE builderToText #-}

strip0x :: MonadFail m => T.Text -> m T.Text
strip0x t = case T.stripPrefix "0x" t of
    Just x -> return x
    Nothing -> fail $ "Missing hex prefix 0x in " <> show t
{-# INLINE strip0x #-}

-- -------------------------------------------------------------------------- --
-- base16-bytestring backward compat

decode16 :: B.ByteString -> Either String B.ByteString
#if MIN_VERSION_base16_bytestring(1,0,0)
decode16 = B16.decode
#else
decode16 bytes = case B16.decode bytes of
    (x, "") -> Right x
    (x, _) -> Left ("invalid character at offset: " <> show (B.length x))
#endif
{-# INLINE decode16 #-}

-- -------------------------------------------------------------------------- --
-- JSON Hex Encoding for Scalar Quantities

newtype HexQuantity a = HexQuantity a
    deriving (Show, Eq)

instance ToJSON (HexQuantity Word8) where
    toJSON (HexQuantity a) = toJSON $ builderToText $ "0x" <> BB.word8Hex a
    toEncoding (HexQuantity a) = unsafeToEncoding $ str ("0x" <> BB.word8Hex a)
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance ToJSON (HexQuantity Word16) where
    toJSON (HexQuantity a) = toJSON $ builderToText $ "0x" <> BB.word16Hex a
    toEncoding (HexQuantity a) = unsafeToEncoding $ str ("0x" <> BB.word16Hex a)
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance ToJSON (HexQuantity Word32) where
    toJSON (HexQuantity a) = toJSON $ builderToText $ "0x" <> BB.word32Hex a
    toEncoding (HexQuantity a) = unsafeToEncoding $ str ("0x" <> BB.word32Hex a)
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance ToJSON (HexQuantity Word64) where
    toJSON (HexQuantity a) = toJSON $ builderToText $ "0x" <> BB.word64Hex a
    toEncoding (HexQuantity a) = unsafeToEncoding $ str ("0x" <> BB.word64Hex a)
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

-- TODO: check that encoding is minimal for all word instances
--
instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => FromJSON (HexQuantity a) where
    parseJSON = withText "HexQuantity" $ strip0x >=> \case
        "0" -> return $ HexQuantity 0
        t
            | T.head t == '0' -> fail $ "Invalid HexQuantity: leading 0 digits. " <> show t
            | otherwise -> case T.hexadecimal @Integer t of
                Right (x, "")
                    | int (minBound @a) <= x && x <= int (maxBound @a) -> return $ HexQuantity $ int x
                    | otherwise -> fail $ "value out of bounds: " <> show x
                Right (x, _) -> fail $ "pending characters after parsing " <> show x
                Left e -> fail e
    {-# INLINEABLE parseJSON #-}
    {-# SPECIALIZE instance FromJSON (HexQuantity Word8) #-}
    {-# SPECIALIZE instance FromJSON (HexQuantity Word16) #-}
    {-# SPECIALIZE instance FromJSON (HexQuantity Word32) #-}
    {-# SPECIALIZE instance FromJSON (HexQuantity Word64) #-}

-- | TODO implement something more efficient
--
instance ToJSON (HexQuantity Natural) where
    toJSON (HexQuantity a) = toJSON $ printf @(Natural -> String) "0x%x" a
    {-# INLINE toJSON #-}

instance {-# OVERLAPPING #-} FromJSON (HexQuantity Natural) where
    parseJSON = withText "HexQuantity" $ \t -> do
        T.hexadecimal <$> strip0x t >>= \case
            Right (x, "") -> return $ HexQuantity x
            Right (x, _) -> fail $ "pending characters after parsing " <> show x
            Left e -> fail e
    {-# INLINE parseJSON #-}

instance {-# OVERLAPPING #-} ToJSON (HexQuantity a) => ToJSON (HexQuantity (Checked u l a)) where
    toJSON (HexQuantity a) = toJSON $ HexQuantity $ unchecked a
    toEncoding (HexQuantity a) = toEncoding $ HexQuantity $ unchecked a
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance {-# OVERLAPPING #-} (KnownChecked u l a, FromJSON (HexQuantity a)) => FromJSON (HexQuantity (Checked u l a)) where
    parseJSON v = do
        HexQuantity a <- parseJSON v
        case checked a of
            Left e -> fail $ show e
            Right x -> return $ HexQuantity x
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- JSON Hex Encoding for Bytes

newtype HexBytes a = HexBytes { _getHexBytes :: a }
    deriving (Show, Eq)

instance ToJSON (HexBytes B.ByteString) where
    toEncoding (HexBytes a) = unsafeToEncoding $ str ("0x" <> BB.byteString (B16.encode a))
    toJSON (HexBytes a) = toJSON $ T.decodeUtf8 $ "0x" <> B16.encode a
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON (HexBytes B.ByteString) where
    parseJSON = withText "HexBytes" $ \t ->
        (decode16 . T.encodeUtf8 <$> strip0x t) >>= \case
            Left e -> fail e
            Right x -> return $ HexBytes x
    {-# INLINE parseJSON #-}

instance ToJSON (HexBytes BS.ShortByteString) where
    toEncoding (HexBytes a) = toEncoding $ HexBytes $ BS.fromShort a
    toJSON (HexBytes a) = toJSON $ HexBytes $ BS.fromShort a
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON (HexBytes BS.ShortByteString) where
    parseJSON v = do
        (HexBytes a) <- parseJSON v
        return (HexBytes $ BS.toShort a)
    {-# INLINE parseJSON #-}

-- -------------------------------------------------------------------------- --
-- Add Label to JSON Parser Context

newtype JsonCtx (l :: Symbol) a = JsonCtx a

instance (KnownSymbol l, FromJSON a) => FromJSON (JsonCtx l a) where
    parseJSON v = JsonCtx <$> parseJSON v <?> Key (fromString $ symbolVal_ @l)

-- -------------------------------------------------------------------------- --
-- Crypto

digestToShortByteString :: Digest a -> BS.ShortByteString
digestToShortByteString d =
#if MIN_VERSION_bytestring(0,10,10)
    unsafePerformIO $ M.withByteArray d $ \ptr -> BS.packCStringLen (ptr, M.length d)
#else
    BS.toShort $! M.convert d
#endif
{-# INLINE digestToShortByteString #-}

-- -------------------------------------------------------------------------- --
-- Binary Encoding for Natural Numbers

encodeLe
    :: Int
        -- ^ minimum length in bytes. The result is padded with 0x0 up to this length.
    -> Natural
        -- ^ The value that is encoded
    -> BB.Builder
encodeLe = go64
  where
    m64 :: Natural
    m64 = 1 + int (maxBound @Word64)

    go64 p n = case quotRem n m64 of
        (0, !r) -> go8 p r
        (!a, !r) -> BB.word64LE (int r) <> go64 (p - 8) a

    go8 p n = case quotRem n (256 :: Natural) of
        (0, !r) -> BB.word8 (int r) <> BB.byteString (B.replicate (p - 1) 0x0)
        (!a, !r) -> BB.word8 (int r) <> go8 (p - 1) a

encodeBe
    :: Int
        -- ^ minimum length in bytes. The result is padded with 0x0 up to this length.
    -> Natural
        -- ^ The value that is encoded
    -> BB.Builder
encodeBe = go64
  where
    m64 :: Natural
    m64 = 1 + int (maxBound @Word64)

    go64 p n = case quotRem n m64 of
        (0, !r) -> go8 p r
        (!a, !r) -> go64 (p - 8) a <> BB.word64LE (int r)

    go8 p n = case quotRem n (256 :: Natural) of
        (0, !r) -> BB.byteString (B.replicate (p - 1) 0x0) <> BB.word8 (int r)
        (!a, !r) -> go8 (p - 1) a <> BB.word8 (int r)

