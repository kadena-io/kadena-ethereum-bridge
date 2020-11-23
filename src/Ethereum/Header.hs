-- |
-- Module: Ethereum.Header
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Ethereum ConsensusHeader
--
module Ethereum.Header
(
-- * Conensus Header
  ConsensusHeader(..)
) where

-- internal modules

import Ethereum.Misc
import Ethereum.RLP

-- -------------------------------------------------------------------------- --
-- Consensus Header

-- [ <hash of previous block>
-- , <uncles hash>,
-- , <miner address>,
-- , <state root hash>,
-- , <transactions root hash>,
-- , <receipts root hash>,
-- , <logs bloom filter>,
-- , <difficulty>,
-- , <number>,
-- , <gas limit>,
-- , <gas used>,
-- , <timestamp>,
-- , <extra data>,
-- , <mix hash>,
-- , <nonce>
-- ]

-- | [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf), 4.3 The Block
--
-- (VERSION 3e2c089 – 2020-09-05)
--
data ConsensusHeader = ConsensusHeader
    { _hdrParentHash :: !ParentHash
        -- ^ \(H_p\) 32 bytes
    , _hdrOmmersHash :: !OmmersHash
        -- ^ \(H_o\) 32 bytes
    , _hdrBeneficiary :: !Beneficiary
        -- ^ \(H_c\) 20 bytes
    , _hdrStateRoot :: !StateRoot
        -- ^ \(H_r\)
    , _hdrTransactionsRoot :: !TransactionsRoot
        -- ^ \(H_t\) 32 bytes
    , _hdrReceiptsRoot :: !ReceiptsRoot
        -- ^ \(H_e\) 32 bytes
    , _hdrLogsBloom :: !Bloom
        -- ^ \(H_b\) 256 bytes
    , _hdrDifficulty :: !Difficulty
        -- ^ \(H_d\) Natural number
    , _hdrNumber :: !BlockNumber
        -- ^ \(H_i\) Natural number
    , _hdrGasLimit :: !GasLimit
        -- ^ \(H_l\) Natural number
    , _hdrGasUsed :: !GasUsed
        -- ^ \(H_g\) Natural number
    , _hdrTimestamp :: !Timestamp
        -- ^ \(H_s\)  \(0 <= H_s < 2^256\)
    , _hdrExtraData :: !ExtraData
        -- ^ \(H_x\) at most 32 bytes
    , _hdrMixHash :: !MixHash
        -- ^ \(H_m\) 32 bytes
    , _hdrNonce :: !Nonce
        -- ^ \(H_n\) 8 bytds
    }
    deriving (Show, Eq)

-- -------------------------------------------------------------------------- --
-- Serialization

-- | LH(H) ≡ ( Hp,Ho,Hc,Hr,Ht,He,Hb,Hd,Hi,Hl,Hg,Hs,Hx,Hm,Hn )
--
instance RLP ConsensusHeader where
    putRlp hdr = putRlpL
        [ putRlp $ _hdrParentHash hdr
        , putRlp $ _hdrOmmersHash hdr
        , putRlp $ _hdrBeneficiary hdr
        , putRlp $ _hdrStateRoot hdr
        , putRlp $ _hdrTransactionsRoot hdr
        , putRlp $ _hdrReceiptsRoot hdr
        , putRlp $ _hdrLogsBloom hdr
        , putRlp $ _hdrDifficulty hdr
        , putRlp $ _hdrNumber hdr
        , putRlp $ _hdrGasLimit hdr
        , putRlp $ _hdrGasUsed hdr
        , putRlp $ _hdrTimestamp hdr
        , putRlp $ _hdrExtraData hdr
        , putRlp $ _hdrMixHash hdr
        , putRlp $ _hdrNonce hdr
        ]

    getRlp = label "ConsensusHeader" $ getRlpL $ ConsensusHeader
        <$> getRlp -- parent hash
        <*> getRlp -- ommers hash
        <*> getRlp -- beneficiary
        <*> getRlp -- state root
        <*> getRlp -- transactions root
        <*> getRlp -- receipts root
        <*> getRlp -- logs bloom
        <*> getRlp -- difficulty
        <*> getRlp -- number
        <*> getRlp -- gas limit
        <*> getRlp -- gas used
        <*> getRlp -- timestamp
        <*> getRlp -- extra data
        <*> getRlp -- mix hash
        <*> getRlp -- nonce

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

