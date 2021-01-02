{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Ethereum.Block
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Ethereum.Block
( Block(..)
, RpcBlock(..)
) where
import Data.Aeson


-- internal modules

import Ethereum.Header
import Ethereum.Misc
import Ethereum.RLP
import Ethereum.Transaction

-- The components in the block are simply the Consensus header, a list of ommer
-- block headers (of the same format as above), BU and a series of the
-- transactions, BT. Formally, we can refer to a block B: \(B ≡ (BH,BT,BU)\)
--
data Block = Block
    { _blockHeader :: !ConsensusHeader
    , _blockTransactions :: ![Transaction]
    , _blockUncles :: ![OmmersHash]
    }
    deriving (Show, Eq)

instance RLP Block where
    putRlp b = putRlpL
        [ putRlp $ _blockHeader b
        , putRlp $ _blockTransactions b
        , putRlp $ _blockUncles b
        ]

    getRlp = label "Block" $ getRlpL $ Block
        <$> getRlp -- header
        <*> getRlp -- transactions
        <*> getRlp -- ommer headers

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- -------------------------------------------------------------------------- --
-- JSON RPC Blocks

data RpcBlock = RpcBlock
    { _rpcBlockHeader :: !ConsensusHeader
    , _rpcBlockTransactions :: ![TransactionHash]
    , _rpcBlockUncles :: ![OmmersHash]
    , _rpcBlockHash :: !BlockHash
    , _rpcTotalDifficulty :: !Difficulty
    , _rpcSize :: !BlockSize
    }
    deriving (Show, Eq)

-- |
--
-- Example from: https://eth.wiki/json-rpc/API#eth_getblockbyhash
--
-- @
--   {
--     "difficulty": "0x4ea3f27bc",
--     "extraData": "0x476574682f4c5649562f76312e302e302f6c696e75782f676f312e342e32",
--     "gasLimit": "0x1388",
--     "gasUsed": "0x0",
--     "hash": "0xdc0818cf78f21a8e70579cb46a43643f78291264dda342ae31049421c82d21ae",
--     "logsBloom": "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
--     "miner": "0xbb7b8287f3f0a933474a79eae42cbca977791171",
--     "mixHash": "0x4fffe9ae21f1c9e15207b1f472d5bbdd68c9595d461666602f2be20daf5e7843",
--     "nonce": "0x689056015818adbe",
--     "number": "0x1b4",
--     "parentHash": "0xe99e022112df268087ea7eafaf4790497fd21dbeeb6bd7a1721df161a6657a54",
--     "receiptsRoot": "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
--     "sha3Uncles": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
--     "size": "0x220",
--     "stateRoot": "0xddc8b0234c2e0cad087c8b389aa7ef01f7d79b2570bccb77ce48648aa61c904d",
--     "timestamp": "0x55ba467c",
--     "totalDifficulty": "0x78ed983323d",
--     "transactions": [
--     ],
--     "transactionsRoot": "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
--     "uncles": [
--     ]
--   }
-- @
--
instance ToJSON RpcBlock where
    toEncoding b = (pairs . mconcat) (blockProperties b)
    toJSON b = object (blockProperties b)
    {-# INLINE toEncoding #-}
    {-# INLINE toJSON #-}

blockProperties :: KeyValue kv => RpcBlock -> [kv]
blockProperties b =
    [ "difficulty" .= _hdrDifficulty (_rpcBlockHeader b)
    , "extraData" .= _hdrExtraData (_rpcBlockHeader b)
    , "gasLimit" .= _hdrGasLimit (_rpcBlockHeader b)
    , "gasUsed" .= _hdrGasUsed (_rpcBlockHeader b)
    , "logsBloom" .= _hdrLogsBloom (_rpcBlockHeader b)
    , "miner" .= _hdrBeneficiary (_rpcBlockHeader b)
    , "mixHash" .= _hdrMixHash (_rpcBlockHeader b)
    , "nonce" .= _hdrNonce (_rpcBlockHeader b)
    , "number" .= _hdrNumber (_rpcBlockHeader b)
    , "parentHash" .= _hdrParentHash (_rpcBlockHeader b)
    , "receiptsRoot" .= _hdrReceiptsRoot (_rpcBlockHeader b)
    , "sha3Uncles" .= _hdrOmmersHash (_rpcBlockHeader b)
    , "stateRoot" .= _hdrStateRoot (_rpcBlockHeader b)
    , "timestamp" .= _hdrTimestamp (_rpcBlockHeader b)
    , "transactions" .= _rpcBlockTransactions b
    , "transactionsRoot" .= _hdrTransactionsRoot (_rpcBlockHeader b)
    , "uncles" .= _rpcBlockUncles b
    , "hash" .= _rpcBlockHash b
    , "totalDifficulty" .= _rpcTotalDifficulty b
    , "size" .= _rpcSize b
    ]

instance FromJSON RpcBlock where
    parseJSON = withObject "Block" $ \o -> RpcBlock
        <$> ( ConsensusHeader
            <$> o .: "parentHash"
            <*> o .: "sha3Uncles"
            <*> o .: "miner"
            <*> o .: "stateRoot"
            <*> o .: "transactionsRoot"
            <*> o .: "receiptsRoot"
            <*> o .: "logsBloom"
            <*> o .: "difficulty"
            <*> o .: "number"
            <*> o .: "gasLimit"
            <*> o .: "gasUsed"
            <*> o .: "timestamp"
            <*> o .: "extraData"
            <*> o .: "mixHash"
            <*> o .: "nonce"
            )
        <*> o .: "transactions"
        <*> o .: "uncles"
        <*> o .: "hash"
        <*> o .: "totalDifficulty"
        <*> o .: "size"

