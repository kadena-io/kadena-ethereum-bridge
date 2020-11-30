{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import Test.Tasty

-- internal modules

import qualified Test.Ethereum.Block
import qualified Test.Ethereum.Ethhash
import qualified Test.Ethereum.HP
import qualified Test.Ethereum.RLP
import qualified Test.Ethereum.Receipt
import qualified Test.Ethereum.Trie
-- import qualified Test.Ethereum.Header

main :: IO ()
main = defaultMain $ testGroup "Ethereum Tests"
    [ Test.Ethereum.RLP.tests
    , Test.Ethereum.Block.tests
    , Test.Ethereum.HP.tests
    , Test.Ethereum.Trie.tests
    , Test.Ethereum.Receipt.tests
    -- , Test.Ethereum.Header.tests
    , Test.Ethereum.Ethhash.tests
    ]

