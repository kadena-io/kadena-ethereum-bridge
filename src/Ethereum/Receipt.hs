{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Ethereum.Receipt
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Transaction Receipt, Yellow Paper 4.3.1
--
module Ethereum.Receipt
( LogTopic(..)
, LogData(..)
, LogEntry(..)
, TxStatus(..)
, Receipt(..)
) where

import qualified Data.ByteString as B

import Numeric.Natural

-- internal modules

import Ethereum.Misc
import Ethereum.RLP

-- -------------------------------------------------------------------------- --
-- Log Entry

newtype LogTopic = LogTopic (BytesN 32)
    deriving (Show)
    deriving newtype (RLP)

newtype LogData = LogData B.ByteString
    deriving (Show)
    deriving newtype (RLP)

data LogEntry = LogEntry
    { _logEntryAddress :: !Address
    , _logEntryTopics :: ![LogTopic]
    , _logEntryData :: !LogData
    }

instance RLP LogEntry where
    putRlp a = putRlpL
        [ putRlp $! _logEntryAddress a
        , putRlpL $! putRlp <$> (_logEntryTopics a)
        , putRlp $! _logEntryData a
        ]
    getRlp = label "LogEntry" $ LogEntry
        <$> getRlp -- address
        <*> getRlp -- topics
        <*> getRlp -- data
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- -------------------------------------------------------------------------- --
-- Tx Status

newtype TxStatus = TxStatus Natural
    deriving (Show)
    deriving newtype (RLP)

-- -------------------------------------------------------------------------- --
-- Receipt

data Receipt = Receipt
    { _receiptGasUsed :: !GasUsed
        -- ^ Gas used in block up to and including this tx.
        --
        -- A non-negative integer value

    , _receiptBloom :: !Bloom
        -- ^ Bloomfilter of the logs
        --
        -- A 2048 bit (256 bytes) hash value

    , _receiptLogs :: ![LogEntry]
        -- ^ Logs that are created during execution of the tx
        --
        -- The sequence Rl is a series of log entries

    , _receiptStatus :: !TxStatus
        -- ^ Status code of the transaction
        --
        -- A non-negative integer
    }

instance RLP Receipt where
    putRlp r = putRlpL
        [ putRlp $! receiptLegacyZeros
        , putRlp $! _receiptGasUsed r
        , putRlp $! _receiptBloom r
        , putRlp $! _receiptLogs r
        , putRlp $! _receiptStatus r
        ]
    getRlp = label "Receipt" $ Receipt
        <$ getRlpB -- legacy zeros
        <*> getRlp -- gas used
        <*> getRlp -- bloom
        <*> getRlp -- logs
        <*> getRlp -- status
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

newtype ReceiptLegacyZeros = ReceiptLegacyZeros (BytesN 32)
    deriving (Show)
    deriving newtype (RLP)

receiptLegacyZeros :: ReceiptLegacyZeros
receiptLegacyZeros = ReceiptLegacyZeros $ replicateN 0x0
{-# INLINE receiptLegacyZeros #-}

