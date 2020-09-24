{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Ethereum.HP
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: BSD-3-Clause
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Appendix C. Hex-Prefix Encoding
--
-- Hex-prefix encoding is an efficient method of encoding an arbitrary number of
-- nibbles as a byte array. It is able to store an additional flag which, when
-- used in the context of the trie (the only context in which it is used),
-- disambiguates between node types.
--
module Ethereum.HP
( hp
) where

import qualified Data.ByteString as B
import Data.Word

-- -------------------------------------------------------------------------- --
-- Hex-Prefix Encoding

hp :: [Word8] -> Bool -> B.ByteString
hp x t
    | even (length x) = B.cons (16 * f t) $ B.pack x
    | otherwise = B.cons (16 * (f t + 1) + head x) $ B.pack (tail x)
  where
    f False = 0
    f True = 2

