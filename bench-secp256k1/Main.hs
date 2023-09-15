{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Control.DeepSeq

import Criterion
import Criterion.Main

-- internal modules

import Crypto.Secp256k1.Internal

-- -------------------------------------------------------------------------- --
-- Orphans

instance NFData Point where
    rnf (Point x y) = x `seq` y `seq` ()
    rnf O = ()

-- -------------------------------------------------------------------------- --
-- Main

-- randomZm :: IO M n
-- randomZm = bytesToZm <$> getEntropy 32

fn1, fn2 :: Fn
fn1 = fn 0xa1cc7887c498a8d4e948be34e9645b2db144dbc4dedb9aedda98c999d97a29ff
fn2 = fn 0x287ab1a7e15775383bf4ac0df141a72d457b39bb2bf9bbe394fd62da26e633cf

fp1 :: Fp
fp1 = fp 0x62995dd98b44b6524410c296ede3dbd80660d0fc9a8750bfd183ca815420e962
-- fp2 :: Fp
-- fp2 = fp 0xc46d0f0d64e22dfb204db3af660ed82c108eec13392dcce8aac41315454317a3

p1 :: Point
p1 = getPublicKey fn1

main :: IO ()
main = defaultMain
    [ bgroup "sqrtFp"
        [ bench "sqrtFp1" $ whnf sqrtFp1 fp1
        , bench "sqrtFp2" $ whnf sqrtFp2 fp1
        , bench "sqrtFp3" $ whnf sqrtFp3 fp1
        ]
    , env (return p1) $ \ ~p ->
        bgroup "multP"
            [ bench "montgomeryMult" $ whnf (montgomeryMult fn2) p
            , bench "doubleAndAdd" $ whnf (doubleAndAdd (nat fn2)) p
            ]
    ]
