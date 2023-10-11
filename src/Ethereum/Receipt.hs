{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

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
, RpcLogEntry(..)
, fromRpcLogEntry
, TxStatus(..)
, Receipt(..)
, TransactionIndex(..)
, RpcReceipt(..)
, fromRpcReceipt
) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as B

import Ethereum.Misc

import Numeric.Natural

-- internal modules

import Ethereum.RLP
import Ethereum.Utils

-- -------------------------------------------------------------------------- --
-- Log Entry

newtype LogTopic = LogTopic (BytesN 32)
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

newtype LogData = LogData B.ByteString
    deriving (Show, Eq)
    deriving newtype (RLP)
    deriving ToJSON via (HexBytes B.ByteString)
    deriving FromJSON via (HexBytes B.ByteString)

data LogEntry = LogEntry
    { _logEntryAddress :: !Address
    , _logEntryTopics :: ![LogTopic]
    , _logEntryData :: !LogData
    }
    deriving (Show, Eq)

instance RLP LogEntry where
    putRlp a = putRlpL
        [ putRlp $! _logEntryAddress a
        , putRlpL $! putRlp <$> _logEntryTopics a
        , putRlp $! _logEntryData a
        ]
    getRlp = label "LogEntry" $ getRlpL $ LogEntry
        <$> label "logEntryAddress" getRlp
        <*> label "logEntryTopics" getRlp
        <*> label "logEntryData" getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance ToJSON LogEntry where
    toEncoding = pairs . mconcat . logEntryProperties
    toJSON = object . logEntryProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON LogEntry where
    parseJSON = withObject "LogEntry" $ \o -> LogEntry
        <$> o .: "address"
        <*> o .: "topics"
        <*> o .: "data"
    {-# INLINE parseJSON #-}

logEntryProperties :: KeyValue e kv => LogEntry -> [kv]
logEntryProperties r =
    [ "address" .= _logEntryAddress r
    , "data" .= _logEntryData r
    , "topics" .= _logEntryTopics r
    ]
{-# INLINE logEntryProperties #-}
{-# SPECIALIZE logEntryProperties :: LogEntry -> [Series] #-}
{-# SPECIALIZE logEntryProperties :: LogEntry -> [Pair] #-}

-- -------------------------------------------------------------------------- --
-- JSON RPC Log Entries

newtype TransactionIndex = TransactionIndex Natural
    deriving (Show, Eq, Ord)
    deriving newtype (RLP)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

data RpcLogEntry = RpcLogEntry
    { _rpcLogEntryAddress :: !Address
        -- ^ 20 Bytes - address from which this log originated.
    , _rpcLogEntryTopics :: ![LogTopic]
        -- ^ Array of 0 to 4 32 Bytes of indexed log arguments. (In solidity: The first topic is the
        -- hash of the signature of the event (e.g. Deposit(address,bytes32,uint256)), except you
        -- declared the event with the anonymous specifier.)
    , _rpcLogEntryData :: !LogData
        -- ^ contains one or more 32 Bytes non-indexed arguments of the log.
    , _rpcLogEntryBlockHash :: !BlockHash
        -- ^ 32 Bytes - hash of the block where this log was in. null when its pending. null when
        -- its pending log.
    , _rpcLogEntryBlockNumber :: !BlockNumber
        -- ^ the block number where this log was in. null when its pending. null when its pending
        -- log.
    , _rpcLogEntryLogIndex :: !TransactionIndex
        -- ^ integer of the log index position in the block. null when its pending log.
    , _rpcLogEntryRemoved :: !Bool
        -- ^ true when the log was removed, due to a chain reorganization. false if it's a valid
        -- log.
    , _rpcLogEntryTransactionHash :: !TransactionHash
        -- ^ 32 Bytes - hash of the transactions this log was created from. null when its pending
        -- log.
    , _rpcLogEntryTransactionIndex :: !TransactionIndex
        -- ^ integer of the transactions index position log was created from. null when its pending
        -- log.
    }
    deriving (Eq, Show)

fromRpcLogEntry :: RpcLogEntry -> LogEntry
fromRpcLogEntry rpc = LogEntry
    { _logEntryAddress = _rpcLogEntryAddress rpc
    , _logEntryTopics = _rpcLogEntryTopics rpc
    , _logEntryData = _rpcLogEntryData rpc
    }

instance ToJSON RpcLogEntry where
    toEncoding = pairs . mconcat . rpcLogEntryProperties
    toJSON = object . rpcLogEntryProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON RpcLogEntry where
    parseJSON = withObject "RpcLogEntry" $ \o -> RpcLogEntry
        <$> o .: "address"
        <*> o .: "topics"
        <*> o .: "data"
        <*> o .: "blockHash"
        <*> o .: "blockNumber"
        <*> o .: "logIndex"
        <*> o .: "removed"
        <*> o .: "transactionHash"
        <*> o .: "transactionIndex"
    {-# INLINE parseJSON #-}

--
-- {
--   "address": "0x06012c8cf97bead5deae237070f9587f8e7a266d",
--   "blockHash": "0xb3b20624f8f0f86eb50dd04688409e5cea4bd02d700bf6e79e9384d47d6a5a35",
--   "blockNumber": "0x5bad55",
--   "data": "0x000000000000000000000000398137383b3d25c92898c656696e41950e47316b00000000000000000000000000000000000000000000000000000000000cee6100000000000000000000000000000000000000000000000000000000000ac3e100000000000000000000000000000000000000000000000000000000005baf35",
--   "logIndex": "0x6",
--   "removed": false,
--   "topics": [
--     "0x241ea03ca20251805084d27d4440371c34a0b85ff108f6bb5611248f73818b80"
--   ],
--   "transactionHash": "0xbb3a336e3f823ec18197f1e13ee875700f08f03e2cab75f0d0b118dabb44cba0",
--   "transactionIndex": "0x11"
-- }
--
rpcLogEntryProperties :: KeyValue e kv => RpcLogEntry -> [kv]
rpcLogEntryProperties r =
    [ "address" .= _rpcLogEntryAddress r
    , "blockHash" .= _rpcLogEntryBlockHash r
    , "blockNumber" .= _rpcLogEntryBlockNumber r
    , "data" .= _rpcLogEntryData r
    , "logIndex" .= _rpcLogEntryLogIndex r
    , "removed" .= _rpcLogEntryRemoved r
    , "topics" .= _rpcLogEntryTopics r
    , "transactionHash" .= _rpcLogEntryTransactionHash r
    , "transactionIndex" .= _rpcLogEntryTransactionIndex r
    ]
{-# INLINE rpcLogEntryProperties #-}
{-# SPECIALIZE rpcLogEntryProperties :: RpcLogEntry -> [Series] #-}
{-# SPECIALIZE rpcLogEntryProperties :: RpcLogEntry -> [Pair] #-}

-- -------------------------------------------------------------------------- --
-- Tx Status

newtype TxStatus = TxStatus Natural
    deriving (Show, Eq)
    deriving ToJSON via (HexQuantity Natural)
    deriving FromJSON via (HexQuantity Natural)

-- | This is the instance used in RLP encodings for Receipts in the Receipt
-- Merkle tree for computing the receipt root in Consensus Headers.
--
-- The Yellow paper doesn't specify how the tx status is encoded in the
-- RLP encoding of receipts. This encoding is derived from
-- <https://github.com/ethereum/go-ethereum/blob/cf856ea1ad96ac39ea477087822479b63417036a/core/types/receipt.go#L36>
--
instance RLP TxStatus where
    putRlp (TxStatus 1) = putRlp @B.ByteString "\x01"
    putRlp (TxStatus 0) = putRlp @B.ByteString ""
    putRlp x = error $ "unsupported tx status: " <> show x

    getRlp = label "TxStatus" $ getRlp @B.ByteString >>= \case
        "\x01" -> return $ TxStatus 1
        "" -> return $ TxStatus 0
        x -> fail $ "unsupported tx status: " <> show x

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- -------------------------------------------------------------------------- --
-- Receipt

data Receipt = Receipt
    { _receiptStatus :: !TxStatus
        -- ^ Status code of the transaction
        --
        -- A non-negative integer

    , _receiptGasUsed :: !GasUsed
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

    }
    deriving (Show, Eq)

instance RLP Receipt where
    putRlp r = putRlpL
        [ putRlp $! _receiptStatus r
        , putRlp $! _receiptGasUsed r
        , putRlp $! _receiptBloom r
        , putRlp $! _receiptLogs r
        ]
    getRlp = label "Receipt" $ getRlpL $ Receipt
        <$> getRlp -- status
        <*> getRlp -- gas used
        <*> getRlp -- bloom
        <*> getRlp -- logs
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

instance ToJSON Receipt where
    toEncoding = pairs . mconcat . receiptProperties
    toJSON = object . receiptProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON Receipt where
    parseJSON = withObject "Receipt" $ \o -> Receipt
        <$> o .: "status"
        <*> o .: "cumulativeGasUsed"
        <*> o .: "bloom"
        <*> o .: "logs"
    {-# INLINE parseJSON #-}

receiptProperties :: KeyValue e kv => Receipt -> [kv]
receiptProperties o =
    [ "status" .= _receiptStatus o
    , "cumulativeGasUsed" .= _receiptGasUsed o
    , "bloom" .= _receiptBloom o
    , "logs" .= _receiptLogs o
    ]
{-# INLINE receiptProperties #-}
{-# SPECIALIZE receiptProperties :: Receipt -> [Series] #-}
{-# SPECIALIZE receiptProperties :: Receipt -> [Pair] #-}

-- -------------------------------------------------------------------------- --
-- JSON RPC API Receipts

data RpcReceipt = RpcReceipt
    { _rpcReceiptGasUsed :: !GasUsed
        -- ^ the amount of gas used by this specific transaction alone.
    , _rpcReceiptBloom :: !Bloom
        -- ^ 256 Bytes - Bloom filter for light clients to quickly retrieve related logs.
    , _rpcReceiptLogs :: ![RpcLogEntry]
        -- ^ Array - Array of log objects, which this transaction generated.
    , _rpcReceiptStatus :: !TxStatus
        -- ^ Status code of the transaction, either 1 (success) or 0 (failure)
        --
        -- For pre Byzantium this is "root", 32 bytes of post-transaction stateroot

    , _rpcReceiptBlockHash :: !BlockHash
        -- ^ 32 Bytes - hash of the block where this transaction was in.
    , _rpcReceiptBlockNumber :: !BlockNumber
        -- ^ block number where this transaction was in.
    , _rpcReceiptContractAddress :: !(Maybe Address)
        -- ^ 20 Bytes - the contract address created, if the transaction was a contract creation, otherwise - null.
    , _rpcReceiptCumulativeGasUsed :: !GasUsed
        -- ^ the total amount of gas used when this transaction was executed in the block.
    , _rpcReceiptFrom :: !Address
        -- ^ 20 Bytes - address of the sender.
    , _rpcReceiptTo :: !(Maybe Address)
        -- ^ 20 Bytes - address of the receiver. Null when the transaction is a contract creation transaction.
    , _rpcReceiptTransactionHash :: !TransactionHash
        -- ^ 32 Bytes - hash of the transaction.
    , _rpcReceiptTransactionIndex :: !TransactionIndex
        -- ^ integer of the transactions index position in the block.
    }
    deriving (Eq, Show)

fromRpcReceipt :: RpcReceipt -> Receipt
fromRpcReceipt rpc = Receipt
    { _receiptGasUsed = _rpcReceiptCumulativeGasUsed rpc
        -- this comes as a surprise, but seems to be required for computing the correct
        -- receipt root in the consensus header.
    -- { _receiptGasUsed = _rpcReceiptGasUsed rpc
    , _receiptBloom = _rpcReceiptBloom rpc
    , _receiptLogs = fromRpcLogEntry <$> _rpcReceiptLogs rpc
    , _receiptStatus = _rpcReceiptStatus rpc
    }

instance ToJSON RpcReceipt where
    toEncoding = pairs . mconcat . rpcReceiptProperties
    toJSON = object . rpcReceiptProperties
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

instance FromJSON RpcReceipt where
    parseJSON = withObject "RpcReceipt" $ \o -> RpcReceipt
        <$> o .: "gasUsed"
        <*> o .: "logsBloom"
        <*> o .: "logs"
        <*> o .: "status"
        <*> o .: "blockHash"
        <*> o .: "blockNumber"
        <*> o .: "contractAddress"
        <*> o .: "cumulativeGasUsed"
        <*> o .: "from"
        <*> o .: "to"
        <*> o .: "transactionHash"
        <*> o .: "transactionIndex"
    {-# INLINE parseJSON #-}

--
-- {
--    "blockHash": "0xb3b20624f8f0f86eb50dd04688409e5cea4bd02d700bf6e79e9384d47d6a5a35",
--    "blockNumber": "0x5bad55",
--    "contractAddress": null,
--    "cumulativeGasUsed": "0xb90b0",
--    "from": "0x398137383b3d25c92898c656696e41950e47316b",
--    "gasUsed": "0x1383f",
--    "logs": [
--      {
--        "address": "0x06012c8cf97bead5deae237070f9587f8e7a266d",
--        "blockHash": "0xb3b20624f8f0f86eb50dd04688409e5cea4bd02d700bf6e79e9384d47d6a5a35",
--        "blockNumber": "0x5bad55",
--        "data": "0x000000000000000000000000398137383b3d25c92898c656696e41950e47316b00000000000000000000000000000000000000000000000000000000000cee6100000000000000000000000000000000000000000000000000000000000ac3e100000000000000000000000000000000000000000000000000000000005baf35",
--        "logIndex": "0x6",
--        "removed": false,
--        "topics": [
--          "0x241ea03ca20251805084d27d4440371c34a0b85ff108f6bb5611248f73818b80"
--        ],
--        "transactionHash": "0xbb3a336e3f823ec18197f1e13ee875700f08f03e2cab75f0d0b118dabb44cba0",
--        "transactionIndex": "0x11"
--      }
--    ],
--    "logsBloom": "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800000200000000000000000000000000000",
--    "status": "0x1",
--    "to": "0x06012c8cf97bead5deae237070f9587f8e7a266d",
--    "transactionHash": "0xbb3a336e3f823ec18197f1e13ee875700f08f03e2cab75f0d0b118dabb44cba0",
--    "transactionIndex": "0x11"
-- }
--
rpcReceiptProperties :: KeyValue e kv => RpcReceipt -> [kv]
rpcReceiptProperties r =
    [ "blockHash" .= _rpcReceiptBlockHash r
    , "blockNumber" .= _rpcReceiptBlockNumber r
    , "contractAddress" .= _rpcReceiptContractAddress r
    , "cumulativeGasUsed" .= _rpcReceiptCumulativeGasUsed r
    , "from" .= _rpcReceiptFrom r
    , "gasUsed" .= _rpcReceiptGasUsed r
    , "logs" .= _rpcReceiptLogs r
    , "logsBloom" .= _rpcReceiptBloom r
    , "status" .= _rpcReceiptStatus r
    , "to" .= _rpcReceiptTo r
    , "transactionHash" .= _rpcReceiptTransactionHash r
    , "transactionIndex" .= _rpcReceiptTransactionIndex r
    ]
{-# INLINE rpcReceiptProperties #-}
{-# SPECIALIZE rpcReceiptProperties :: RpcReceipt -> [Series] #-}
{-# SPECIALIZE rpcReceiptProperties :: RpcReceipt -> [Pair] #-}

