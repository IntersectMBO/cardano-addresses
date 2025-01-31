{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Options.Applicative.Private
    (
    -- * Types
      PrivateType (..)

    -- * Applicative Parser
    , privateOpt
    ) where

import Prelude

import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag', long )


data PrivateType = ChainCode | SigningKey
    deriving (Eq, Show)

--
-- Applicative Parser
--

-- | Parse an 'PrivateType' from the command-line, as set of non-overlapping flags.
privateOpt :: Parser PrivateType
privateOpt = cc <|> signing
  where
    cc = flag' ChainCode (long "chain-code")
    signing = flag' SigningKey (long "signing-key")
