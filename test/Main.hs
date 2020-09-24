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

import qualified Test.Ethereum.RLP
import qualified Test.Ethereum.Block

main :: IO ()
main = defaultMain $ testGroup "Ethereum Tests"
    [ Test.Ethereum.RLP.tests
    , Test.Ethereum.Block.tests
    ]

