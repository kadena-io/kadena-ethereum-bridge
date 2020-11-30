{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import System.Clock

-- internal modules

import Ethereum.Ethhash

-- -------------------------------------------------------------------------- --
--

main :: IO ()
main = do

    -- This are longrunning benchmarks
    bench "createCache 0" $ bench_createCache 0
    bench "createCache 1" $ bench_createCache 1
    bench "createCache 2" $ bench_createCache 2
    bench "createCache 3" $ bench_createCache 3
    bench "createCache 100" $ bench_createCache 100
    bench "createCache 200" $ bench_createCache 200
    bench "createCache 300" $ bench_createCache 300
    bench "createCache 400" $ bench_createCache 400
    bench "createCache 500" $ bench_createCache 500

bench :: String -> IO a -> IO ()
bench label run = do
    putStr $ label <> ": "
    (t, !_) <- stopWatch run
    putStrLn $ show t


stopWatch :: IO a -> IO (TimeSpec, a)
stopWatch run = do
    t0 <- getTime ThreadCPUTime
    !r <- run
    t1 <- getTime ThreadCPUTime
    return (diffTimeSpec t1 t0, r)

-- -------------------------------------------------------------------------- --
-- Benchmarks

bench_createCache :: Epoch -> IO Cache
bench_createCache = createCache

