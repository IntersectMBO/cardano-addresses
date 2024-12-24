{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Governance
    (
    -- * Types
      GovernanceType (..)

    -- * Applicative Parser
    , governanceOpt
    ) where

import Prelude

import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag', long )


data GovernanceType = WithByte | WithoutByte
    deriving (Eq, Show)

--
-- Applicative Parser
--

-- | Parse an 'GovernanceType' from the command-line, if there is proper flag then 'with byte' is set.
governanceOpt :: Parser GovernanceType
governanceOpt = withByte <|> pure WithoutByte
  where
    withByte = flag' WithByte (long "with-byte")
