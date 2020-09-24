{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Ethereum.Trie
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Appendix D. Modified Merkle Patricia Tree
--
-- The modified Merkle Patricia tree (trie) provides a persistent data structure
-- to map between arbitrary-length binary data (byte arrays). It is defined in
-- terms of a mutable data structure to map between 256-bit binary fragments and
-- arbitrary-length binary data, typically implemented as a database. The core
-- of the trie, and its sole requirement in terms of the protocol specification,
-- is to provide a single value that identifies a given set of key-value pairs,
-- which may be either a 32-byte sequence or the empty byte sequence. It is left
-- as an implementation consideration to store and maintain the structure of the
-- trie in a manner that allows effective and efficient realisation of the
-- protocol.
--
module Ethereum.Trie
(
) where

-- -------------------------------------------------------------------------- --
-- Trie

-- Nodes

newtype Nibbles = Nibbles BS.ShortByteString

data TrieNode
    = LeafNode {-# UNPACK #-} !Nibbles {-# UNPACK #-} !B.ByteString
        -- ^ A two-item structure whose first item corresponds to the nibbles in
        -- the key not already accounted for by the accumulation of keys and
        -- branches traversed from the root. The hex-prefix encoding method is
        -- used and the second parameter to the function is required to be 1.

    | ExtensionNode {-# UNPACK #-} !Nibbles {-# UNPACK #-} !B.ByteString
        -- ^ A two-item structure whose first item corresponds to a series of
        -- nibbles of size greater than one that are shared by at least two
        -- distinct keys past the accumulation of the keys of nibbles and the
        -- keys of branches as traversed from the root. The hex-prefix encoding
        -- method is used and the second parameter to the function is required
        -- to be 0.

    | BranchNode {-# UNPACK #-} !(V.Vector (BytesN 32)) !(Maybe B.ByteString)
        -- ^ A 17-item structure whose first sixteen items correspond to each of
        -- the sixteen possible nibble values for the keys at this point in
        -- their traversal. The 17th item is used in the case of this being a
        -- terminator node and thus a key being ended at this point in its
        -- traversal.

c :: [(B.ShortByteString, B.ShortByteString)] ->

-- Node encodings

data NodeCap
    = EmptyNode
    | HashNode Keccak256Hash
    | LiteralNode B.ShortByteString -- at most 32 bytes

n :: [(B.ShortByteString, B.ShortByteString)] -> NodeCap
n [] = EmptyNode
n l
    | length cl < 32 = LiteralNode cl
    | otherwise = HashNode $ keccak256 cl
  where
    cl = c l

-- trie

trie :: [(B.ShortByteString, B.ShortByteString)] -> Trie
trie l = keccak256 c(l, 0)

