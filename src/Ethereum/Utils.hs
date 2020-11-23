{-# LANGUAGE DataKinds #-}
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
-- TODO
--
module Ethereum.Utils
( int
, pow256
-- * Cryptography
, digestToShortByteString
-- * JSON
, HexQuantity(..)
, HexBytes(..)
, JsonCtx(..)
) where

import Control.Monad

import Crypto.Hash (Digest)

import Data.Aeson
import Data.Aeson.Encoding hiding (int)
import Data.Aeson.Internal
import qualified Data.ByteArray as M (length, withByteArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Word

import GHC.TypeLits (KnownSymbol, Symbol, symbolVal')

import Numeric.Natural

import System.IO.Unsafe

import Text.Printf

import GHC.Exts (proxy#)




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

-- -------------------------------------------------------------------------- --
-- JSON

str :: BB.Builder -> BB.Builder
str b = "\"" <> b <> "\""
{-# INLINE str #-}

builderToText :: BB.Builder -> T.Text
builderToText = T.decodeUtf8 . BL.toStrict . BB.toLazyByteString
{-# INLINE builderToText #-}

strip0x :: MonadFail m => T.Text -> m T.Text
strip0x t = case T.stripPrefix "0x" t of
    Just x -> return x
    Nothing -> fail $ "Missing hex prefix 0x in " <> show t
{-# INLINE strip0x #-}

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
    {-# INLINE parseJSON #-}

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
        B16.decode . T.encodeUtf8 <$> strip0x t >>= \case
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
    parseJSON v = JsonCtx <$> parseJSON v <?> Key (T.pack $ symbolVal' (proxy# @l))

-- -------------------------------------------------------------------------- --
-- Crypto

digestToShortByteString :: Digest a -> BS.ShortByteString
digestToShortByteString d = unsafePerformIO $
    M.withByteArray d $ \ptr -> BS.packCStringLen (ptr, M.length d)
{-# INLINE digestToShortByteString #-}

