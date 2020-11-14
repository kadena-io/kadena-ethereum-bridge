{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
( Trie(..)
, trie
, createTrie
, lookupTrie
, TrieException(..)
, TrieStore(..)
, mkHashMapStore

-- * TrieNode
, TrieNode(..)

-- * Proofs
, Proof(..)
, createProof
, lookupTrieWithProof
, validateProof
) where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- internal modules

import Ethereum.HP
import Ethereum.Misc
import Ethereum.RLP

-- -------------------------------------------------------------------------- --
-- Trie

-- | Trie Node
--
-- It is not at all clear to me why 'EmptyNode', 'EmptyCap', and empty values
-- branch nodes are encoded as @putRlp ""@ and not as @putRlp ()@. The latter
-- would have allowed storing empty values in the trie. With the choice of the
-- former that is not possible.
--
data TrieNode
    = EmptyNode
    | LeafNode {-# UNPACK #-} !Nibbles {-# UNPACK #-} !BS.ShortByteString
        -- ^ A two-item structure whose first item corresponds to the nibbles in
        -- the key not already accounted for by the accumulation of keys and
        -- branches traversed from the root. The hex-prefix encoding method is
        -- used and the second parameter to the function is required to be 1.

    | ExtensionNode {-# UNPACK #-} !Nibbles {-# UNPACK #-} !NodeCap
        -- ^ A two-item structure whose first item corresponds to a series of
        -- nibbles of size greater than one that are shared by at least two
        -- distinct keys past the accumulation of the keys of nibbles and the
        -- keys of branches as traversed from the root. The hex-prefix encoding
        -- method is used and the second parameter to the function is required
        -- to be 0.

        -- QUESTION: What happens if for keys A, B, C, the key A is a proper prefix of B
        -- and C? Will A become a leaf and B and C extensions? Or will there be
        -- a common extension followed by at key A?
        --
        -- ANSWER: the yellowpaper states that:
        -- 1. For leaf nodes all key prefixes since the last branch are unique.
        -- 2. Extension nodes are of length at least 1
        --
        -- Hence, if A is a proper prefix of B and C, there must be a branching
        -- node for the last common nibble of A, B, and C.

    | BranchNode {-# UNPACK #-} !(V.Vector NodeCap) {-# UNPACK #-} !(Maybe BS.ShortByteString)
        -- ^ A 17-item structure whose first sixteen items correspond to each of
        -- the sixteen possible nibble values for the keys at this point in
        -- their traversal. The 17th item is used in the case of this being a
        -- terminator node and thus a key being ended at this point in its
        -- traversal.
    deriving (Show, Eq)

instance RLP TrieNode where
    putRlp EmptyNode = putRlp @B.ByteString mempty
    putRlp (LeafNode n b) = putRlp (FlaggedNibbles True n, b)
    putRlp (ExtensionNode n b) = putRlp (FlaggedNibbles False n, b)
    putRlp (BranchNode vs b) = putRlp
        ( vs V.! 0
        , vs V.! 1
        , vs V.! 2
        , vs V.! 3
        , vs V.! 4
        , vs V.! 5
        , vs V.! 6
        , vs V.! 7
        , vs V.! 8
        , vs V.! 9
        , vs V.! 10
        , vs V.! 11
        , vs V.! 12
        , vs V.! 13
        , vs V.! 14
        , vs V.! 15
        , fromMaybe mempty b
        )
    {-# INLINE putRlp #-}

    -- TODO avoid backtracking and detect node type more efficiently.
    --
    -- It is not clear if, when parsing branchNodes, it's possible to fail fast.
    --
    getRlp = label "TrieNode"
        $ label "EmptyNode" (EmptyNode <$ getRlpBSize 0)
        <|> branchNode
        <|> otherNode
      where
        -- otherNode = getRlpLSize (<= 32) $ do
        otherNode = getRlpL $ do
            FlaggedNibbles f ns <- getRlp
            if f
              then label "LeafNode" (LeafNode ns <$> getRlp)
              else label "ExtensionNode" (ExtensionNode ns <$> getRlp)

        branchNode = label "BranchNode" $ getRlpL $ BranchNode
            <$> label "branches" (V.replicateM 16 getRlp)
            <*> label "value" (do
                getRlp @BS.ShortByteString >>= \case
                    "" -> return Nothing
                    a -> return (Just a)
                )

    {-# INLINE getRlp #-}

-- -------------------------------------------------------------------------- --

-- | Representation of Trie Nodes. Nodes with less than 32 bytes are represented
-- inline in the Trie. Larger nodes are referenced by they Keccak256 hash.
--
data NodeCap
    = EmptyCap
    | HashCap !Keccak256Hash
    | LiteralCap !TrieNode {- Lazy -} Put -- less than 32 bytes

instance Eq NodeCap where
    EmptyCap == EmptyCap = True
    (HashCap x) == (HashCap y) = x == y
    (LiteralCap x _) == (LiteralCap y _) = x == y
    _ == _ = False

instance Show NodeCap where
    show EmptyCap = "EmptyCap"
    show (HashCap h) = "(HashCap " <> show h <> ")"
    show (LiteralCap n _) = "(LiteralCap (" <> show n <> "))"

instance RLP NodeCap where
    putRlp EmptyCap = putRlp @B.ByteString mempty
    putRlp (HashCap h) = putRlp h
    putRlp (LiteralCap _ p) = p

    getRlp = label "NodeCap"
        $ label "EmptyCap" (EmptyCap <$ getRlpBSize 0)
        <|> label "HashCap" (HashCap <$> getRlp)
        <|> label "LiteralCap" (getRlp >>= \n -> return $ LiteralCap n ({- Lazy -} putRlp n))

    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

-- | Reference a Trie node. Small nodes are inlined. Other nodes are referenced
-- by their respective Keccak256 Hash.
--
cap
    :: (Keccak256Hash -> B.ByteString -> IO ())
        -- ^ key-value storage funtion
    -> TrieNode
        -- ^ The node that is to be encoded
    -> IO NodeCap
        -- ^ Node Reference
cap store x
    | byteCount p < 32 = do
        logg $ "literal: " <> show x <> "\n"
        return $ LiteralCap x p
    | otherwise = HashCap h <$ store h e <* loggStore
  where
    loggStore = logg $ "== " <> show h <> " ==> " <> show x <> "\n"
    h = keccak256 e
    p = putRlp x
    e = putByteString p

-- -------------------------------------------------------------------------- --
-- Trie

data TrieException
    = RootNotFound !Trie
    | NodeNotFound !Keccak256Hash
    | EmptyNodeCapLookup
    | InvalidTrieNode T.Text
    | ConflictingEntries Nibbles B.ByteString B.ByteString
    deriving (Show)

instance Exception TrieException

-- The root of a Trie.
--
-- This data type only stores a reference to the root node. It doesn't capture
-- the underlying storage backend.
--
newtype Trie = Trie Keccak256Hash
    deriving (Show)

-- | Create a trie that stores the given key-value pairs.
--
trie
    :: (Keccak256Hash -> B.ByteString -> IO ())
        -- ^ Key-value storage callback for persisting the trie nodes
    -> [(B.ByteString, B.ByteString)]
        -- ^ The key-value pairs that are stored in the trie
    -> IO Trie
trie store l = do
    root <- createTrie store l
    let n = putRlpByteString root
        h = keccak256 n
    logg $ "root: " <> show root <> "\n"
    store h n
    return $ Trie h

-- | Lookup the value for a given key in a trie.
--
-- If not value is stored for the given key 'Nothing' is returned.
--
-- If the Trie is inconsistent an exception of type 'TrieException' is thrown.
--
-- TODO: use createProof to implement this?
--
lookupTrie
    :: (Keccak256Hash -> IO (Maybe B.ByteString))
    -> Trie
    -> B.ByteString
    -> IO (Maybe B.ByteString)
lookupTrie store (Trie rootHash) rootKey = do

    -- lookup initial node
    store rootHash >>= \case
        Nothing -> throw $ RootNotFound (Trie rootHash)
        Just x -> decodeNode x >>= go (toNibbles rootKey)

  where

    -- Traverse key nibbles
    go :: Nibbles -> TrieNode -> IO (Maybe B.ByteString)
    go _ EmptyNode = return Nothing

    go key (LeafNode k v)
        | key == k = return (Just $ BS.fromShort v)
        | otherwise = logg (show key <> " /= " <> show k) >> return Nothing

    go key (ExtensionNode k c) = case nstripPrefix k key of
        Nothing -> return Nothing
        Just x -> lookupCap c >>= go x

    go key (BranchNode bs v)
        | nnull key = return $ BS.fromShort <$> v
        | otherwise = case bs V.! fromIntegral (getHexDigit (nhead key)) of
            EmptyCap -> return Nothing
            c -> lookupCap c >>= go (ndrop 1 key)

    -- resolve node caps
    lookupCap :: NodeCap -> IO TrieNode
    lookupCap EmptyCap = throw EmptyNodeCapLookup
    lookupCap (HashCap h) = store h >>= \case
        Nothing -> throw $ NodeNotFound h
        Just x -> decodeNode x
    lookupCap (LiteralCap n _) = return n -- decodeNode (BS.fromShort v)

    -- RLP decode a trie node
    decodeNode :: B.ByteString -> IO TrieNode
    decodeNode x = case get getRlp x of
        Left e -> throwM $ InvalidTrieNode $ T.pack e
        Right y -> return y

nisPrefixOf :: Nibbles -> Nibbles -> Bool
nisPrefixOf a b = ntoList a `L.isPrefixOf` ntoList b

nstripPrefix :: Nibbles -> Nibbles -> Maybe Nibbles
nstripPrefix a b
    | nisPrefixOf a b = Just $ ndrop (nlength a) b
    | otherwise = Nothing

-- -------------------------------------------------------------------------- --
-- Create Trie

-- | Create a Trie Node
--
createTrie
    :: (Keccak256Hash -> B.ByteString -> IO ())
        -- ^ key-value storage function for storing trie nodes.
        -- (Node that those are different form the input key-value pairs.)
    -> [(B.ByteString, B.ByteString)]
        -- ^ Key-value pairs the are stored in the Trie
    -> IO TrieNode
createTrie store l = go (L.sort $ first toNibbles <$> l)
    -- we sort once at the beginning. Instead we could have sorted also just on
    -- the first nibble each time before calling 'L.groupBy' below. At least in
    -- theory, sorting in the beginning leads to a more efficient
    -- implementation.
  where

    -- group all keys by first nibble
    --
    -- the use of 'L.groupBy' assumes that the keys are sorted by the first
    -- nibble.
    --
    go :: [(Nibbles, B.ByteString)] -> IO TrieNode
    go ls = logg (show ls) >> do
      n <- case L.groupBy ((==) `on` (nmaybeHead . fst)) ls of

        [[]] -> error "impossible"

        -- Empty Trie. Note that this can only occur when the trie is empty.
        [] -> do
            return EmptyNode

        -- Leaf Node
        [[(a,b)]] -> do
            return $ LeafNode a (BS.toShort b)

        -- ExtensionNode (h0 /= mempty, because otherwise xs would be singleton)
        [xs@((h0,_):_)] | not (nnull h0) -> do
            let e = extend xs -- find end of common prefix (at least 1)
                rest = (_1 %~ ndrop e) <$> xs -- strip common prefix
            nc <- go rest >>= cap store
            return $ ExtensionNode (ntake e h0) nc

        -- branch
        xs -> do
            branches <- VM.replicate 16 EmptyCap
            ref <- newIORef Nothing

            -- Add branches for first nibble of each key group
            forM_ xs $ \case
                [] -> error "impossible"

                -- Key ends here and value is included in branch node
                [(a,b)] | nnull a -> do
                    writeIORef ref (Just $ BS.toShort b)

                -- Catch the case when the same key appearce more than once
                g@((h0,b):(_,b1):_) | nnull h0 -> if
                    | allEqual (snd <$> g) -> do
                        writeIORef ref (Just $ BS.toShort b)
                    | otherwise -> throwM $ ConflictingEntries h0 b b1

                -- add branch for nibble
                g@((h0,_):_) -> do
                    let rest = (_1 %~ ndrop 1) <$> g
                    inner <- go rest
                    let nibble = nhead h0
                    nc <- cap store inner
                    VM.unsafeWrite branches (fromIntegral $ getHexDigit nibble) nc

            -- Finalize branch node
            bs <- V.unsafeFreeze branches
            content <- readIORef ref
            return $ BranchNode bs content
      return n

    -- find longest common prefix for set of key suffixes with same first nibble
    extend :: [(Nibbles, B.ByteString)] -> Int
    extend xs = pref 1
      where
        pref i
            | all (> i) (nlength . fst <$> xs) && allEqual ((flip nix i . fst) <$> xs) = pref (i + 1)
            | otherwise = i

-- -------------------------------------------------------------------------- --
-- Utils

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (h:t) = all (== h) t
{-# INLINE allEqual #-}

logg :: String -> IO ()
-- logg = putStrLn
logg _ = return ()

data TrieStore = TrieStore
    { _trieStoreAdd :: Keccak256Hash -> B.ByteString -> IO ()
    , _trieStoreLookup :: Keccak256Hash -> IO (Maybe B.ByteString)
    }

mkHashMapStore :: IO TrieStore
mkHashMapStore = do
    ref <- newIORef @(HM.HashMap Keccak256Hash B.ByteString) mempty
    return $ TrieStore
        { _trieStoreAdd = \k -> modifyIORef' ref . HM.insert k
        , _trieStoreLookup = \k -> HM.lookup k <$> readIORef ref
        }

-- -------------------------------------------------------------------------- --
--  Proofs

data Proof = Proof
    { _proofKey :: !B.ByteString
    , _proofValue :: !(Maybe B.ByteString)
    , _proofNodes :: ![B.ByteString]
    , _proofRoot :: !Keccak256Hash
    }
    deriving (Show, Eq)

instance RLP Proof where
    putRlp p@(Proof { _proofValue = Nothing }) = putRlp
        ( _proofKey p
        , () -- use of () (instead of "") allows to distinguish between non-existing keys and ""
        , _proofNodes p
        , _proofRoot p
        )
    putRlp p@(Proof { _proofValue = Just x }) = putRlp
        ( _proofKey p
        , x
        , _proofNodes p
        , _proofRoot p
        )

    getRlp = label "Proof" $ getRlpL $ Proof
        <$> label "proofKey" getRlp
        <*> label "proofValue" val
        <*> label "proofNodes" getRlp
        <*> label "proofRoot" getRlp
      where
        val = isL >>= \case
            True -> Nothing <$ getRlp @()
            False -> Just <$> getRlp @B.ByteString
    {-# INLINE putRlp #-}
    {-# INLINE getRlp #-}

createProof
    :: [(B.ByteString, B.ByteString)]
        -- ^ Key-value pairs that are stored in the Trie
    -> B.ByteString
        -- ^ The key for the proof
    -> IO Proof
createProof ps key = do
    s <- mkHashMapStore
    t <- trie (_trieStoreAdd s) ps
    lookupTrieWithProof (_trieStoreLookup s) True t key


type DList a = [a] -> [a]

-- | Lookup the value for a given key in a trie.
--
-- If not value is stored for the given key 'Nothing' is returned.
--
-- If the Trie is inconsistent an exception of type 'TrieException' is thrown.
--
lookupTrieWithProof
    :: (Keccak256Hash -> IO (Maybe B.ByteString))
    -> Bool
    -> Trie
    -> B.ByteString
    -> IO Proof
lookupTrieWithProof rawStore validate t@(Trie rootHash) rootKey = do

    -- lookup initial node
    store rootHash >>= \case
        Nothing -> throw $ RootNotFound t
        Just x -> do
            rootNode <- decodeNode x
            (nodes, value) <- go (toNibbles rootKey) rootNode (x:)
            return $ Proof
                { _proofKey = rootKey
                , _proofValue = value
                , _proofNodes = nodes
                , _proofRoot = rootHash
                }
  where
    store h
        | validate = rawStore h >>= \case
            Nothing -> return Nothing
            Just x
                | keccak256 x == h -> return $ Just x
                | otherwise -> throwM $ InvalidTrieNode
                    $ "The hash " <> T.pack (show h) <> " does not match the node " <> T.pack (show x)
        | otherwise = store h

    -- Traverse key nibbles
    go :: Nibbles -> TrieNode -> DList B.ByteString -> IO ([B.ByteString], Maybe B.ByteString)
    go _ EmptyNode acc = return (acc [], Nothing)

    go key (LeafNode k v) acc
        | key == k = return $ (acc [], Just (BS.fromShort v))
        | otherwise = return $ (acc [], Nothing)

    go key (ExtensionNode k c) acc = case nstripPrefix k key of
        Nothing -> return (acc [], Nothing)
        Just x -> do
            (node, acc') <- lookupCap c acc
            go x node acc'

    go key (BranchNode bs v) acc
        | nnull key = return $ (acc [], (BS.fromShort <$> v))
        | otherwise = case bs V.! fromIntegral (getHexDigit (nhead key)) of
            EmptyCap -> return (acc [], Nothing)
            c -> do
                (node, acc') <- lookupCap c acc
                go (ndrop 1 key) node acc'

    -- resolve node caps
    lookupCap :: NodeCap -> DList B.ByteString -> IO (TrieNode, DList B.ByteString)
    lookupCap EmptyCap _ = throw EmptyNodeCapLookup
    lookupCap (HashCap h) acc = store h >>= \case
        Nothing -> throw $ NodeNotFound h
        Just x -> (, acc . (x:)) <$> decodeNode x
    lookupCap (LiteralCap n _) acc = return (n, acc) -- decodeNode (BS.fromShort v)

    -- RLP decode a trie node
    decodeNode :: B.ByteString -> IO TrieNode
    decodeNode x = case get getRlp x of
        Left e -> throwM $ InvalidTrieNode $ T.pack e
        Right y -> return y

-- | Proof validation works by using the nodes in the proof as a partial trie
-- and looking up the key in that trie while validating the hashes of the nodes.
-- At the end it is verified that the proof value of the lookup matches the
-- value from the iput proof.
--
validateProof
    :: Proof
    -> IO Bool
validateProof p = do
    s <- mkHashMapStore
    forM_ (_proofNodes p) $ \n ->
        (_trieStoreAdd s) (keccak256 n) n
    p' <- lookupTrieWithProof (_trieStoreLookup s) True (Trie $ _proofRoot p) (_proofKey p)
    return $ _proofValue p == _proofValue p'
