{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Ethereum.Receipt.ReceiptProof
-- Copyright: Copyright © 2021 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Ethereum Receipt Proofs
--
module Ethereum.Receipt.ReceiptProof
(
-- * Receipt Proofs
  ReceiptProofException(..)
, ReceiptProof(..)
, rpcReceiptProof
, ReceiptProofValidation(..)
, validateReceiptProof

-- ** Internal
, receiptTrie
, rpcReceiptTrie
, rpcReceiptTrieProof
) where

import Control.Monad
import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Bifunctor
import qualified Data.ByteString as B

import Ethereum.Misc

-- internal modules

import Ethereum.Header
import Ethereum.RLP
import Ethereum.Receipt
import Ethereum.Trie
import Ethereum.Utils

-- -------------------------------------------------------------------------- --
-- Receipt Trie

rpcReceiptTrie
    :: (Keccak256Hash -> B.ByteString -> IO ())
        -- ^ Key-value storage callback for persisting the trie nodes
    -> [RpcReceipt]
    -> IO Trie
rpcReceiptTrie store rs = receiptTrie store
    $ (\x -> (_rpcReceiptTransactionIndex x, fromRpcReceipt x)) <$> rs

receiptTrie
    :: (Keccak256Hash -> B.ByteString -> IO ())
        -- ^ Key-value storage callback for persisting the trie nodes
    -> [(TransactionIndex, Receipt)]
        -- ^ block receipts
    -> IO Trie
receiptTrie store receipts = trie store
    $ bimap putRlpByteString putRlpByteString <$> receipts

rpcReceiptTrieProof
    :: [RpcReceipt]
    -> TransactionIndex
    -> Proof
rpcReceiptTrieProof rs = receiptTrieProof kv
  where
    kv = (\x -> (_rpcReceiptTransactionIndex x, fromRpcReceipt x)) <$> rs

receiptTrieProof
    :: [(TransactionIndex, Receipt)]
        -- ^ block receipts
    -> TransactionIndex
    -> Proof
receiptTrieProof receipts idx = createProof kv (putRlpByteString idx)
  where
    kv = bimap putRlpByteString putRlpByteString <$> receipts

-- -------------------------------------------------------------------------- --
-- Receipt Proof
--
-- Receipts are relatively large data structures. For many applications it may
-- be sufficient to only create proof for individual LogEntries, ideally in form
-- of 'RpcLogEntry'. However, that's not directly supported by the Ethereum
-- on-chain data structures.

-- | Receipt Proof.
--
-- The proof does not include the proof root. The reason is that the root must
-- be computed and trust must be established during validation. Including it
-- into the proof would be redundant or even misleading.
--
data ReceiptProof = ReceiptProof
    { _receiptProofTrie :: !Proof
        -- ^ Merkle proof that witnesses that the receipt is contained in the
        -- ReceiptRoot of the first provided ConsensusHeader.
    , _receiptProofHeader :: !ConsensusHeader
        -- ^ The consensus header of the block that contains the receipt.
    , _receiptProofExtraHeaders :: ![ConsensusHeader]
        -- ^ A number of consecutive consensus headers where the first header
        -- is the successor of the '_receiptProofHeader'.
    }
    deriving (Show, Eq)

instance RLP ReceiptProof where
    putRlp p = putRlp
        ( _receiptProofTrie p
        , _receiptProofHeader p
        , _receiptProofExtraHeaders p
        )
    getRlp = label "ReceiptProof" $ getRlpL $ ReceiptProof
        <$> label "receiptProofTrie" getRlp
        <*> label "receiptProofHeader" getRlp
        <*> label "receiptProofExtraHeader" getRlp
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

data ReceiptProofException
    = ReceiptProofRootMismatch
    | ReceiptProofBrokenHeaderChain
    | ReceiptProofInvalidTrie
    | ReceiptProofKeyDecodingFailure
    | ReceiptProofValueDecodingFailure
    deriving (Show, Eq)

instance Exception ReceiptProofException where
    displayException ReceiptProofRootMismatch = "receipts root of receipts Merkle tree doesn't match receipts root in header"
    displayException ReceiptProofBrokenHeaderChain = "the consensus header chain is broken"
    displayException ReceiptProofInvalidTrie = "the receipt proof trie is invalid"
    displayException ReceiptProofKeyDecodingFailure = "failed to decode the proof key"
    displayException ReceiptProofValueDecodingFailure = "failed to decode the proof value"

rpcReceiptProof
    :: MonadThrow m
    => ConsensusHeader
    -> [ConsensusHeader]
    -> [RpcReceipt]
    -> TransactionIndex
    -> m ReceiptProof
rpcReceiptProof hdr extraHdrs receipts txid = do

    -- Verify that Merkle root matches receipts root in the header
    unless (bytes (_proofRoot rp) == bytes (_hdrReceiptsRoot hdr)) $
        throwM ReceiptProofRootMismatch

    -- Verify the header chain
    void $ checkHeaderChain (blockHash hdr) extraHdrs

    return $! ReceiptProof
        { _receiptProofTrie = rp
        , _receiptProofHeader = hdr
        , _receiptProofExtraHeaders = extraHdrs
        }
  where
    rp = rpcReceiptTrieProof receipts txid

checkHeaderChain
    :: MonadThrow m
    => BlockHash
    -> [ConsensusHeader]
    -> m BlockHash
checkHeaderChain = foldM checkHeader
  where
    checkHeader parent hdr
        | bytes (_hdrParentHash hdr) == bytes parent = return $! blockHash hdr
        | otherwise = throwM ReceiptProofBrokenHeaderChain

-- -------------------------------------------------------------------------- --
-- Receipt Proof Validation

-- | The result of evaluting a receipt proof.
--
-- The function witnesses that '_receiptProofValidationReceipt' exists at
-- transaction with index '_receiptProofValidationIndex' in the block with the
-- header '_receiptProofHeader' that is the ancestor at depth
-- '_receiptProofValidationDepth' of a block with block hash
-- '_receiptProofValidationRoot'.
--
-- The value of '_receiptProofValidationWeight' indicates the PoW weight that is
-- included in the proof. This value may be zero if the validator doesn't
-- support PoW validation. It is up to the caller to decide which weight is
-- sufficient for trusting the '_receiptProofValidationRoot'. In case of
-- insufficient weight some oracle must be consulted in order to certify that
-- the root is included in the chain.
--
-- The value of '_receiptProofValidationDepth' witnesses a lower bound on the
-- depth in the chain at which the receipt is included with respect to the root,
-- i.e. the value of '_receiptProofValidationRoot'. A larger depth increases the
-- confidence that the receipt won't be orphaned.
--
data ReceiptProofValidation = ReceiptProofValidation
    { _receiptProofValidationRoot :: !BlockHash
        -- ^ The root of the proof. This is not necessarily the block that
        -- contains the receipt, but the latest block in the chain of consensus
        -- headers that are included in the proof. The validation certifies that
        -- the block that contains the receipt is an ancestor of the root on the
        -- block chain.
    , _receiptProofValidationDepth :: !BlockDepth
        -- ^ The depth of the proof i.e. the length of the chain of consensus
        -- headers that are included in the proof. This number is zero if no
        -- extra headers are provided in the proof.
    , _receiptProofValidationWeight :: !Difficulty
        -- ^ The proof-of-work weight of the proof, i.e. the sum of the
        -- difficulties of all headers. This can be zero when proof isn't backed
        -- by any PoW weight or when the validator doesn't support PoW
        -- validation. If this is zero it means that the root is not trusted.
        -- The user must consult a chain header oracle to verify that the root
        -- is contained in the block chain.
    , _receiptProofValidationHeader :: !ConsensusHeader
        -- ^ The consensus header of the block that contains the receipt
    , _receiptProofValidationIndex :: !TransactionIndex
        -- ^ The transaction index of the transaction of the receipt.
    , _receiptProofValidationReceipt :: !Receipt
        -- ^ The receipt that is proven to be included in the block chain.
    }
    deriving (Show, Eq)

instance ToJSON ReceiptProofValidation where
    toJSON = object . receiptProofValidationProperties
    toEncoding = pairs . mconcat . receiptProofValidationProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance FromJSON ReceiptProofValidation where
    parseJSON = withObject "ReceiptProofValidation" $ \o -> ReceiptProofValidation
        <$> o .: "root"
        <*> o .: "depth"
        <*> o .: "weight"
        <*> o .: "header"
        <*> o .: "index"
        <*> o .: "receipt"
    {-# INLINE parseJSON #-}

receiptProofValidationProperties :: KeyValue e kv => ReceiptProofValidation -> [kv]
receiptProofValidationProperties o =
    [ "root" .= _receiptProofValidationRoot o
    , "depth" .= _receiptProofValidationDepth o
    , "weight" .= _receiptProofValidationWeight o
    , "header" .= _receiptProofValidationHeader o
    , "index" .= _receiptProofValidationIndex o
    , "receipt" .= _receiptProofValidationReceipt o
    ]
{-# INLINE receiptProofValidationProperties #-}
{-# SPECIALIZE receiptProofValidationProperties :: ReceiptProofValidation -> [Series] #-}
{-# SPECIALIZE receiptProofValidationProperties :: ReceiptProofValidation -> [Pair] #-}

validateReceiptProof
    :: MonadThrow m
    => ReceiptProof
    -> m ReceiptProofValidation
validateReceiptProof proof = do
    -- validate receipt Merkle root
    t <- validateProof (_receiptProofTrie proof)
    unless t $ throwM ReceiptProofInvalidTrie

    -- Verify that Merkle root matches receipts root in the header
    unless (bytes (_proofRoot p) == bytes (_hdrReceiptsRoot hdr)) $
        throwM ReceiptProofRootMismatch

    -- Verify the header chain
    root <- checkHeaderChain (blockHash hdr) $ _receiptProofExtraHeaders proof

    -- decode proof key and value
    idx <- case get getRlp (_proofKey p) of
        Left _ -> throwM ReceiptProofKeyDecodingFailure
        Right x -> return x
    receipt <- case _proofValue p of
        Nothing -> throwM ReceiptProofKeyDecodingFailure
        Just x -> case get getRlp x of
            Left _ -> throwM ReceiptProofValueDecodingFailure
            Right y -> return y

    return ReceiptProofValidation
        { _receiptProofValidationRoot = root
        , _receiptProofValidationDepth = int $ length $ _receiptProofExtraHeaders proof
        , _receiptProofValidationWeight = 0
        , _receiptProofValidationIndex = idx
        , _receiptProofValidationHeader = hdr
        , _receiptProofValidationReceipt = receipt
        }

  where
    hdr = _receiptProofHeader proof
    p = _receiptProofTrie proof
