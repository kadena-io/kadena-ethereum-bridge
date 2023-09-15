{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
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

import qualified Test.Crypto.Secp256k1.Internal

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Crypto.Secp256k1"
    [ Test.Crypto.Secp256k1.Internal.tests
    ]
