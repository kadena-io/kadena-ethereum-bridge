{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Ethereum.Ethhash
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Ethhash implementation as described in <https://eth.wiki/en/concepts/ethash/ethash> and
-- <https://github.com/ethereum/go-ethereum/blob/master/consensus/ethash/algorithm.go>.
--
-- FIXME: the current implementation assumes little endian host byte order. Add code
-- to adjust for bigendian platforms.
--
module Ethereum.Ethhash
( BlockNumber(..)
, Epoch(..)
, getEpoch
, Cache(..)
, createCache
, createCacheForBlockNumber
, generateDatasetItem
, hashimotoLight

-- Cache and data set sizes
, getCacheSize
, getDataSize

-- * Internal Utils (mostly for testing)
, seed
, mkCacheBytes
, newKeccak256Ctx
, newKeccak512Ctx
, hash
, fnv
, fnvHash
, Seed(..)
) where

import Control.Exception
import Control.Monad

import Crypto.Hash (Keccak_256(..), Keccak_512(..))
import Crypto.Hash.IO

import Data.Aeson
import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Unsafe as BU
import Data.Word

import Foreign.Marshal.Alloc (mallocBytes, allocaBytes, free)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.Stack
import GHC.TypeLits

import System.IO.Unsafe

-- internal modules

import Ethereum.Ethhash.CacheSizes
import Ethereum.Ethhash.DataSizes
import Ethereum.Misc
import Ethereum.Utils

-- -------------------------------------------------------------------------- --
-- Utils

-- | Allocate an ByteString and initialize the memory with the provided
-- callback.
--
allocateByteString :: Int -> (Ptr a -> IO ()) -> IO B.ByteString
allocateByteString n act = mask $ \umask -> do
    !ptr <- mallocBytes @Word8 n
    flip onException (free ptr) $ do
        umask (act $ castPtr ptr)
        BU.unsafePackMallocCStringLen (castPtr ptr, n)
{-# INLINE allocateByteString #-}

allocateShortByteString :: Int -> (Ptr a -> IO ()) -> IO BS.ShortByteString
allocateShortByteString n act = allocaBytes n $ \ptr -> do
    act $ castPtr ptr
    BS.packCStringLen (ptr, n)
{-# INLINE allocateShortByteString #-}

allocateBytesN
    :: forall n a
    . HasCallStack
    => KnownNat n
    => (Ptr a -> IO ())
    -> IO (BytesN n)
allocateBytesN act = do
    !bs <- allocateShortByteString (int $ natVal' (proxy# @n)) act
    case bytesN bs of
        Left e -> error $ "Ethereum.Ethhash.allocateBytesN: Internal invariant violoated: " <> e
        Right !x -> return x
{-# INLINE allocateBytesN #-}

-- -------------------------------------------------------------------------- --
-- Hash Functions

newKeccak256Ctx :: IO (MutableContext Keccak_256)
newKeccak256Ctx = hashMutableInit
{-# INLINE newKeccak256Ctx #-}

newKeccak512Ctx :: IO (MutableContext Keccak_512)
newKeccak512Ctx = hashMutableInit
{-# INLINE newKeccak512Ctx #-}

-- | Compute a hash
--
hash
    :: forall a
    . HashAlgorithm a
    => MutableContext a
        -- ^ (non-shareable) hash context
    -> Ptr Word8
        -- ^ buffer
    -> Int
        -- ^ buffer size of the input buffer in bytes
    -> Ptr Word8
        -- ^ hash, must point to a memory of alt least 'hashDigestSize @a' bytes
    -> IO ()
hash ctx buf bufSize h = do
    hashMutableReset ctx
    BA.withByteArray ctx $ \ctxPtr -> do
        hashInternalUpdate @a ctxPtr buf $ fromIntegral bufSize
        hashInternalFinalize ctxPtr $ castPtr h
{-# INLINE hash #-}

-- -------------------------------------------------------------------------- --
-- | FNV

-- fnv is an algorithm inspired by the FNV hash, which in some cases is used as
-- a non-associative substitute for XOR. Note that we multiply the prime with
-- the full 32-bit input, in contrast with the FNV-1 spec which multiplies the
-- prime with one byte (octet) in turn.

fnvPrime :: Word32
fnvPrime = 0x01000193
{-# INLINE fnvPrime #-}

fnv :: Word32 -> Word32 -> Word32
fnv v1 v2 = ((v1 * fnvPrime) `xor` v2)
{-# INLINE fnv #-}

fnvHash
    :: Int
        -- ^ number of 32bit hashes
    -> Ptr Word32
        -- ^ target memory into which the result is mixed
    -> Ptr Word32
        -- ^ data that is mixed into the target. The point is not
        -- modified.
    -> IO ()
fnvHash n v1 v2 = forM_ [0 .. n-1] $ \i -> do
    x <- fnv <$> peekElemOff v1 i <*> peekElemOff v2 i
    pokeElemOff v1 i x
{-# INLINE fnvHash #-}

-- -------------------------------------------------------------------------- --
-- Constants

-- | blocks per epoch
--
epochLength :: Int
epochLength = 30000

-- | number of rounds in cache production
--
cacheRounds :: Int
cacheRounds = 3

-- | number of accesses in hashimoto loop
--
accesses :: Int
accesses = 64

-- | Number of cache values that are used in the computation of a single dataset
-- item
--
dataSetParents :: Int
dataSetParents = 256 -- Number of parents of each dataset element

-- -------------------------------------------------------------------------- --
-- Epoch

newtype Epoch = Epoch Int
    deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

getEpoch :: BlockNumber -> Epoch
getEpoch b = int $ int b `quot` epochLength
{-# INLINE getEpoch #-}

-- -------------------------------------------------------------------------- --
-- Cache Seed Generation

newtype Seed = Seed (BytesN 32)
    deriving (Show, Eq, Bytes, Storable)
    deriving ToJSON via (HexBytes (BytesN 32))
    deriving FromJSON via (HexBytes (BytesN 32))

-- | The seed to use for generating a verification cache and the mining dataset.
--
seed
    :: HasCallStack
    => Epoch
        -- ^ epoch
    -> IO Seed
seed 0 = return $ Seed $! replicateN 0x0
seed epoch = fmap Seed $ allocateBytesN $ \ptr -> do
    fillBytes ptr 0x0 32
    ctx <- newKeccak256Ctx
    replicateM_ (int epoch) $! hash ctx ptr 32 ptr

-- -------------------------------------------------------------------------- --
-- Cache Generation

-- | 32 MB representing a set of 524288 64-byte values.
--
data Cache = Cache
    { _cacheSize :: {-# UNPACK #-} !Int
        -- ^ The size of the cache in bytes
    , _cacheEpoch :: {-# UNPACK #-} !Epoch
        -- ^ The epoch of the cache
    , _cacheBytes :: {-# UNPACK #-} !B.ByteString
        -- ^ The cache data
    }

instance Show Cache where
    show (Cache s e b) = "Cache "
        <> show s
        <> " " <> show e
        <> " " <> B8.unpack (B16.encode (B.take 512 b))
        <> if B.null (B.drop 512 b) then "" else "..."

createCacheForBlockNumber :: BlockNumber -> IO Cache
createCacheForBlockNumber = createCache . getEpoch
{-# INLINE createCacheForBlockNumber #-}

createCache :: Epoch -> IO Cache
createCache epoch = Cache size epoch <$> (mkCacheBytes size =<< seed epoch)
  where
    size = getCacheSize epoch

-- generateCache creates a verification cache of a given size for an input seed.
-- The cache production process involves first sequentially filling up 32 MB of
-- memory, then performing two passes of Sergio Demian Lerner's RandMemoHash
-- algorithm from Strict Memory Hard Hashing Functions (2014).
--
-- This method places the result into dest in machine byte order.
--
mkCacheBytes
    :: Int
        -- Cache size in bytes (cf. getCacheSize)
    -> Seed
        -- Seed (cf. seed)
    -> IO B.ByteString
mkCacheBytes cacheSize (Seed s) = allocateByteString cacheSize $ \ptr -> do
    !ctx <- newKeccak512Ctx

    -- Sequentially produce the initial dataset
    BS.useAsCStringLen (_getBytesN s) $ \(seedPtr,x) ->
        () <$ hash ctx (castPtr seedPtr) x ptr
    forM_ [hashBytes, (2*hashBytes) .. cacheSize - 1] $ \i ->
        hash ctx (plusPtr ptr (i - hashBytes)) hashBytes (plusPtr ptr i)

    -- Use a low-round version of randmemohash
    allocaBytes @Word64 hashBytes $ \tmpPtr -> do
        forM_ [0..cacheRounds - 1] $ \_ -> do
            forM_ [0..n-1] $ \i -> do

                let srcPtr = plusPtr ptr $ ((i - 1 + n) `rem` n) * hashBytes
                let dstPtr = plusPtr ptr $ i * hashBytes

                dstWord <- peek @Word32 dstPtr
                let xorPtr = plusPtr ptr $ int @Word32 @Int (dstWord `rem` int n) * hashBytes

                forM_ [0 .. (hashBytes `quot` 8) - 1] $ \o -> do
                    x <- xor
                        <$> peekElemOff @Word64 srcPtr o
                        <*> peekElemOff @Word64 xorPtr o
                    pokeElemOff @Word64 tmpPtr o x

                void $! hash ctx (castPtr tmpPtr) hashBytes dstPtr
                return ()
  where
    hashBytes = hashDigestSize Keccak_512

    -- Cache size in 32 bit words
    n = cacheSize `quot` hashBytes

-- -------------------------------------------------------------------------- --
-- DataSet

-- | 64 Bytes. This is a Keccak512 hash, but we store it as plain (pinned)
-- ByteString because for the purpose of verification it's computed on the fly
-- and pinning it avoids some memcopy operations.
--
newtype DatasetItem = DatasetItem B.ByteString
    deriving (Show, Eq, Bytes)
    deriving FromJSON via (HexBytes B.ByteString)
    deriving ToJSON via (HexBytes B.ByteString)

-- | Compute an item in the full dataset from the cache.
--
-- The resulting 'B.BytesString' is a Keccak512Hash with of 64 bytes.
--
generateDatasetItem
    :: Cache
        -- ^ Cache
    -> Int
        -- ^ Index of the item in the data set
    -> IO DatasetItem
generateDatasetItem cache idx = fmap DatasetItem $

    -- cache is indexed as 64 blocks
    -- mix has 64 bytes.
    -- fnv operators on 4 byte blocks
    --
    allocateByteString hashBytes $ \mixPtr8 -> do
        let mixPtr32 = castPtr @_ @Word32 mixPtr8
        unsafeWithPtr (_cacheBytes cache) $ \cachePtr8 _ -> do
            let cachePtr32 = castPtr @_ @Word32 cachePtr8
            copyBytes mixPtr8 (plusPtr cachePtr8 ((idx `rem` n) * hashBytes)) 64
            x <- peek mixPtr32
            poke mixPtr32 (x `xor` int idx)
            ctx512 <- newKeccak512Ctx
            hash ctx512 mixPtr8 hashBytes mixPtr8
            forM_ [0 .. dataSetParents - 1] $ \j -> do
                !y <- peekElemOff mixPtr32 (j `rem`  r)
                let !cacheIndex = (fnv (xor (int idx) (int j)) y) `rem` (int n)
                fnvHash r mixPtr32 (plusPtr cachePtr32 (int cacheIndex * hashBytes))
            hash ctx512 mixPtr8 hashBytes mixPtr8
  where
    hashBytes = 64
    wordBytes = 4

    -- cache size in bytes (for 64 byte based indexing)
    n = _cacheSize cache `quot` hashBytes

    -- for 4 byte (Word32) based indexing
    r = hashBytes `quot` wordBytes

-- -------------------------------------------------------------------------- --
-- Hashimoto
--
-- hashimoto aggregates data from the full dataset in order to produce our final
-- value for a particular header hash and nonce.
--

-- | The inner mining loop.
--
-- For verification a slower lookup function can be used the recomputes the
-- dataset entries from the smaller cache. For mining that would be too slow and
-- instead the full dataset is computed once and cached.
--
-- The intermediate compressed mix digest is returned as a fail-fast pre
-- verification step in order to prevent DOS attacks. A verifier would first
-- verify that the Keccak256 hash of the mix-hash along with the seed satisfies
-- the target. Only if that holds the verifier would verify the the mix hash.
--
-- The fail-fast mix-hash verification can also used in stateless network
-- components, such as proxies or reverse proxies to prevent DOS attacks.
--
hashimoto
    :: BlockHash
        -- ^ block hash
    -> Nonce
        -- ^ nonce
    -> Int
        -- ^ size of full dataset (cacheSize * 64)
    -> (Int -> IO DatasetItem)
        -- ^ dataset lookup callback
    -> IO (MixHash, Keccak256Hash)
        -- ^ The
        --
        -- 1. intermediate compressed mix digest and
        -- 2. final POW hash
        --
hashimoto block (Nonce nonce) size lup =
    -- Start the mix with replicated seed
    -- (seed has 64 bytes and mix has 128 bytes)
    withPtr (bytes seed512 <> bytes seed512) $ \mixPtr8 _ -> do

        let !mixPtr32 = castPtr @_ @Word32 mixPtr8

        !seed0 <- unsafeWithPtr seed512 $ \ptr _ -> peek @Word32 (castPtr ptr)

        -- mix in random dataset nodes
        forM_ [0..accesses - 1] $ \i -> do
            !x <- peekElemOff mixPtr32 (i `rem` mixWords32)
            let !p = (int (fnv (xor (int i) seed0) x) `rem` (n `quot` mixHashes)) * mixHashes
            !a0 <- lup (int p) -- returns B.ByteString of 64 bytes
            unsafeWithPtr a0 $ \a0Ptr _ ->
                fnvHash 16 mixPtr32 (castPtr a0Ptr)
            !a1 <- lup (int p + 1)
            unsafeWithPtr a1 $ \a1Ptr _ ->
                fnvHash 16 (plusPtr mixPtr32 64) (castPtr a1Ptr)

        -- compress mix
        --
        -- mix has 128 bytes, cmix has 32 bytes.
        -- mix has 32 Word32, cmix has 8 Word32.
        -- compression packs 4 Word32 into a single Word32
        --
        !cmix <- allocateBytesN $ \cmixPtr32 -> do
            forM_ [0..7] $ \i -> do
                let curc = advancePtr cmixPtr32 i
                let curm = advancePtr mixPtr32 (i * 4)
                copyBytes curc (advancePtr curm 0) 4
                fnvHash 1 curc (advancePtr curm 1)
                fnvHash 1 curc (advancePtr curm 2)
                fnvHash 1 curc (advancePtr curm 3)

        let !result = keccak256 (bytes seed512 <> bytes cmix)
        return (MixHash cmix, result)
  where
    -- the number of bytes in a 512bit hash (64)
    hashBytes = hashDigestSize Keccak_512

    -- number of bytes in the mix hash (128)
    mixBytes = 128

    -- number of 32bit words in the mix hash (32)
    mixWords32 = mixBytes `quot` 4

    -- number of 512bit hashes in the full dataset
    n = size `quot` hashBytes

    -- number of 512bit hashes in the mix hash (2)
    mixHashes = mixBytes `quot` hashBytes

    -- Combine header+nonce into a 64 byte seed
    seed512 = keccak512 $ bytes block <> bytes nonce

-- hashimotoLight aggregates data from the full dataset (using only a small
-- in-memory cache) in order to produce our final value for a particular header
-- hash and nonce.
--
hashimotoLight
    :: Int
        -- ^ size of full dataset (cacheSize * 64)
    -> Cache
        -- ^ Cache
    -> BlockHash
        -- ^ Block hash
    -> Nonce
        -- ^ Nonce
    -> IO (MixHash, Keccak256Hash)
hashimotoLight size cache block nonce = hashimoto block nonce size lup
  where
    lup i = generateDatasetItem cache i

-- -------------------------------------------------------------------------- --
-- Data and Cache sizes

{-
def get_cache_size(block_number):
    sz = CACHE_BYTES_INIT + CACHE_BYTES_GROWTH * (block_number // EPOCH_LENGTH)
    sz -= HASH_BYTES
    while not isprime(sz / HASH_BYTES):
        sz -= 2 * HASH_BYTES
    return sz

def get_full_size(block_number):
    sz = DATASET_BYTES_INIT + DATASET_BYTES_GROWTH * (block_number // EPOCH_LENGTH)
    sz -= MIX_BYTES
    while not isprime(sz / MIX_BYTES):
        sz -= 2 * MIX_BYTES
    return sz
-}

getDataSize :: HasCallStack => Epoch -> Int
getDataSize (Epoch i) = unsafePerformIO $
    unsafeWithPtr dataSizes $ \ptr l -> do
        when (i > l - 1) $
            error $ "Ethereum.Ethhash.getDataSize: epoch index " <> show i <> " out of bounds"
        peekElemOff @Int (castPtr ptr) i

getCacheSize :: Epoch -> Int
getCacheSize (Epoch i) = unsafePerformIO $
    unsafeWithPtr cacheSizes $ \ptr l -> do
        when (i > l - 1) $
            error $ "Ethereum.Ethhash.getDataSize: epoch index " <> show i <> " out of bounds"
        peekElemOff @Int (castPtr ptr) i

