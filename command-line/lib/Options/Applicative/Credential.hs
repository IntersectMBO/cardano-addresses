{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Credential
    ( delegationCredentialArg
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..), liftXPub )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )
import Options.Applicative.Derivation
    ( xpubReader )
import Options.Applicative.Script
    ( scriptHashReader )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

--
-- Applicative Parser
--

delegationCredentialArg  :: String -> Parser (Credential 'DelegationK)
delegationCredentialArg helpDoc = argument (eitherReader reader) $ mempty
    <> metavar "KEY || SCRIPT HASH"
    <> help helpDoc
  where
    reader :: String -> Either String (Credential 'DelegationK)
    reader str =
       (DelegationFromKey . liftXPub <$> xpubReader allowedPrefixes str)
       `orElse`
       (DelegationFromScript <$> scriptHashReader str)
       `orElse`
       Left "Couldn't parse delegation credentials. Neither a public key nor a script hash."

    -- TODO: Allow non-extended keys here.
    allowedPrefixes =
        [ CIP5.stake_xvk
        ]

    orElse :: Either e result -> Either e result -> Either e result
    orElse _1st@Right{} _2nd = _1st
    orElse _1st@Left{}  _2nd = _2nd
