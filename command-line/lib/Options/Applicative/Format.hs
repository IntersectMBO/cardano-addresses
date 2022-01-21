{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Format
    (
    -- * Types
      FormatType (..)

    -- * Applicative Parser
    , formatOpt
    ) where

import Prelude

import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag', long )


data FormatType = Hex | Bech32
    deriving (Eq, Show)

--
-- Applicative Parser
--

-- | Parse an 'FormatType' from the command-line, if there is proper flag then hex is set.
formatOpt :: Parser FormatType
formatOpt = hex <|> pure Bech32
  where
    hex = flag' Hex (long "hex")
