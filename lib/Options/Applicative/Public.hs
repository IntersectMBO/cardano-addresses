{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Options.Applicative.Public
    (
    -- * Types
      PublicType (..)

    -- * Applicative Parser
    , publicOpt
    ) where

import Prelude

import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag', long )


data PublicType = WithChainCode | WithoutChainCode
    deriving (Eq, Show)

--
-- Applicative Parser
--

-- | Parse an 'PublicType' from the command-line, as set of non-overlapping flags.
publicOpt :: Parser PublicType
publicOpt = withoutCC <|> withCC
  where
    withoutCC = flag' WithoutChainCode (long "without-chain-code")
    withCC = flag' WithChainCode (long "with-chain-code")
