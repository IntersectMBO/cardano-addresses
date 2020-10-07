{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Credential
    (
    -- * Types
      CredentialType (..)

    -- * Applicative Parser
    , credentialOpt
    ) where

import Cardano.Address.Style.Shelley
    ( CredentialType (..) )
import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag', long )

--
-- Applicative Parser
--

-- | Parse an 'CredentialType' from the command-line, as set of non-overlapping flags.
credentialOpt :: Parser CredentialType
credentialOpt = fromKeyFlag <|> fromScriptFlag
  where
    fromKeyFlag = flag' CredentialFromKey (long "from-key")
    fromScriptFlag = flag' CredentialFromScript (long "from-script")
