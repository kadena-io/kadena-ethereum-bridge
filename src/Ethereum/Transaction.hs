{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module: Ethereum.Transaction
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- 4.2. The Transaction
--
module Ethereum.Transaction
(
-- * ECDSA
  EcdsaSignature(..)
, secp256k1n

-- * Misc
, TransactionData(..)
, CodeFragment(..)
, TransactionNonce(..)
, GasPrice(..)
, TransactionGasLimit(..)
, Wei(..)

-- * Transaction
, Transaction(..)
) where

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Aeson.Types (JSONPathElement(Key))
import qualified Data.ByteString.Short as BS
import Data.Word

import Ethereum.Utils

import GHC.Natural

-- internal modules

import Ethereum.Misc
import Ethereum.RLP

-- -------------------------------------------------------------------------- --
-- ECDSA Signature
--
-- TODO

data EcdsaSignature = EcdsaSignature
    { _sigV :: !Word8
        -- ^ \(v ∈ {27, 28}\)
    , _sigR :: !(BytesN 32)
        -- ^ \(0 < r < secp256k1n\)
    , _sigS :: !(BytesN 32)
        -- ^ \(0 < s < secp256k1n÷2+1\)
    }

secp256k1n :: Natural
secp256k1n = 115792089237316195423570985008687907852837564279074904382605163141518161494337

-- -------------------------------------------------------------------------- --
--

newtype TransactionData = TransactionData BS.ShortByteString
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes BS.ShortByteString)
    deriving FromJSON via (HexBytes BS.ShortByteString)

newtype CodeFragment = CodeFragment BS.ShortByteString
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes BS.ShortByteString)
    deriving FromJSON via (HexBytes BS.ShortByteString)

newtype TransactionNonce = TransactionNonce Word256
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Word256)
    deriving FromJSON via (HexQuantity Word256)

newtype GasPrice = GasPrice Word256
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Word256)
    deriving FromJSON via (HexQuantity Word256)

newtype TransactionGasLimit = TransactionGasLimit Word256
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Word256)
    deriving FromJSON via (HexQuantity Word256)

newtype Wei = Wei Word256
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Word256)
    deriving FromJSON via (HexQuantity Word256)

-- -------------------------------------------------------------------------- --
-- Transaction

data Transaction
    = MessageCall
        { _transactionNonce :: !TransactionNonce
            -- ^  A scalar value equal to the number of transactions sent by the
            -- sender; formally \(T_n\).
        , _transactionGasPrice :: !GasPrice
            -- ^ A scalar value equal to the number of Wei to be paid per unit
            -- of gas for all computation costs incurred as a result of the
            -- execution of this transaction; formally \(T_p\).
        , _transactionGasLimit :: !TransactionGasLimit
            -- ^ A scalar value equal to the maximum amount of gas that should
            -- be used in executing this transaction. This is paid up-front,
            -- before any computation is done and may not be increased later;
            -- formally \(T_g\).
        , _transactionTo :: !Address
            -- ^ The 160-bit address of the message call’s recipient or, for a
            -- contract creation transaction, \(\emptyset\), used here to denote
            -- the only member of \(B_0\) ; formally \(T_t\).
        , _transactionValue :: !Wei
            -- ^ A scalar value equal to the number of Wei to be transferred to
            -- the message call’s recipient or, in the case of contract
            -- creation, as an endowment to the newly created account; formally
            -- \(T_v\).
        , _transactionV :: !Word8
            -- ^  element of \(N_5\)
        , _transactionR :: !Word256
        , _transactionS :: !Word256
            -- ^ Values corresponding to the signature of the transaction and
            -- used to determine the sender of the transaction; formally
            -- \(T_w\), \(T_r\) and \(T_s\). This is expanded in Appendix F.
        , _transactionData :: !TransactionData
            -- ^ data: An unlimited size byte array specifying the input data of
            -- the message call, formally \(T_d\).
        }

    | ContractCreation
        { _transactionNonce :: !TransactionNonce
            -- ^  A scalar value equal to the number of transactions sent by the
            -- sender; formally \(T_n\).
        , _transactionGasPrice :: !GasPrice
            -- ^ A scalar value equal to the number of Wei to be paid per unit
            -- of gas for all computation costs incurred as a result of the
            -- execution of this transaction; formally \(T_p\).
        , _transactionGasLimit :: !TransactionGasLimit
            -- ^ A scalar value equal to the maximum amount of gas that should
            -- be used in executing this transaction. This is paid up-front,
            -- before any computation is done and may not be increased later;
            -- formally \(T_g\).
        , _transactionValue :: !Wei
            -- ^ A scalar value equal to the number of Wei to be transferred to
            -- the message call’s recipient or, in the case of contract
            -- creation, as an endowment to the newly created account; formally
            -- \(T_v\).
        , _transactionV :: !Word8
        , _transactionR :: !Word256
        , _transactionS :: !Word256
            -- ^ Values corresponding to the signature of the transaction and
            -- used to determine the sender of the transaction; formally
            -- \(T_w\), \(T_r\) and \(T_s\). This is expanded in Appendix F.
        , _transactionInit :: !CodeFragment
            -- ^ An unlimited size byte array specifying the EVM-code for the
            -- account initialisation procedure, formally \(T_i\).
        }
        deriving (Show, Eq)

instance RLP Transaction where
    putRlp r@MessageCall{} = putRlpL
        [ putRlp $ _transactionNonce r
        , putRlp $ _transactionGasPrice r
        , putRlp $ _transactionGasLimit r
        , putRlp $ _transactionTo r
        , putRlp $ _transactionValue r
        , putRlp $ _transactionV r
        , putRlp $ _transactionR r
        , putRlp $ _transactionS r
        , putRlp $ _transactionData r
        ]
    putRlp r@ContractCreation{} = putRlpL
        [ putRlp $ _transactionNonce r
        , putRlp $ _transactionGasPrice r
        , putRlp $ _transactionGasLimit r
        , putRlp $ _transactionValue r
        , putRlp $ _transactionV r
        , putRlp $ _transactionR r
        , putRlp $ _transactionS r
        , putRlp $ _transactionInit r
        ]

    getRlp = label "Transaction" $ getRlpL $ do
        nonce <- getRlp
        price <- getRlp
        limit <- getRlp
        maybeTo <- Just <$> getRlp <|> Nothing <$ getRlpBSize 0
        case maybeTo of
            Nothing -> ContractCreation nonce price limit
                <$> getRlp -- Value
                <*> getRlp -- v
                <*> getRlp -- r
                <*> getRlp -- s
                <*> getRlp -- init
            Just to -> MessageCall nonce price limit to
                <$> getRlp -- Value
                <*> getRlp -- v
                <*> getRlp -- r
                <*> getRlp -- s
                <*> getRlp -- data

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance ToJSON Transaction where
    toEncoding = pairs . mconcat . transactionProperties
    toJSON = object . transactionProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

transactionProperties :: KeyValue kv => Transaction -> [kv]
transactionProperties r@MessageCall{} =
    [ "nonce" .= _transactionNonce r
    , "gasPrice" .= _transactionGasPrice r
    , "gasLimit" .= _transactionGasLimit r
    , "to" .= _transactionTo r
    , "value" .= _transactionValue r
    , "v" .= _transactionV r
    , "r" .= _transactionR r
    , "s" .= _transactionS r
    , "data" .= _transactionData r
    ]
transactionProperties r@ContractCreation{} =
    [ "nonce" .= _transactionNonce r
    , "gasPrice" .= _transactionGasPrice r
    , "gasLimit" .= _transactionGasLimit r
    , "value" .= _transactionValue r
    , "v" .= _transactionV r
    , "r" .= _transactionR r
    , "s" .= _transactionS r
    , "init" .= _transactionInit r
    ]
{-# INLINE transactionProperties #-}
{-# SPECIALIZE transactionProperties :: Transaction -> [Series] #-}
{-# SPECIALIZE transactionProperties :: Transaction -> [Pair] #-}

instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \o -> do
        nonce <- o .: "nonce"
        price <- o .: "gasPrice"
        limit <- o .: "gasLimit"
        maybeTo <- optional (o .: "to")
        case maybeTo of
            Nothing -> ContractCreation nonce price limit
                <$> o .: "value" -- Value
                <*> o .: "v" -- v
                <*> o .: "r" -- r
                <*> o .: "s" -- s
                <*> o .: "init" -- init
                <?> Key "ContractCreation"
            Just to -> MessageCall nonce price limit to
                <$> o .: "value" -- Value
                <*> o .: "v" -- v
                <*> o .: "r" -- r
                <*> o .: "s" -- s
                <*> o .: "data" -- data
                <?> Key "MessageCall"
    {-# INLINE parseJSON #-}
