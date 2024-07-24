{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

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
