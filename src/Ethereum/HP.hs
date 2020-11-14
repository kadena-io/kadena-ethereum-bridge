{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
(
-- * Hex Digits
  HexDigit
, upper
, lower
, hexDigits
, getHexDigit

-- * Nibbles
, Nibbles
, nibbles
, toNibbles
, nnull
, nlength
, nix
, nhead
, nmaybeHead
, nsplitAt
, ndrop
, ntake
, nrange
, ntoList

-- ** Internal Tools
, nalign
, checkNibbles
, nix_
, nhead_

-- * Hex Prefix Encoding
, FlaggedNibbles(..)
, HpException(..)
, toHp
, fromHp
) where

-- internal modules

import Ethereum.HP.Internal

