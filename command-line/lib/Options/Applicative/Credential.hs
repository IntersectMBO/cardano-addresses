{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Credential
    ( delegationCredentialArg
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..) )
import Cardano.Address.Internal
    ( orElse )
import Cardano.Address.Script
    ( KeyRole (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..), liftXPub )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )
import Options.Applicative.Derivation
    ( keyhashReader, xpubReader )
import Options.Applicative.Script
    ( scriptHashReader )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

--
-- Applicative Parser
--

delegationCredentialArg  :: String -> Parser (Credential 'DelegationK)
delegationCredentialArg helpDoc = argument (eitherReader reader) $ mempty
    <> metavar "KEY || KEY HASH || SCRIPT HASH"
    <> help helpDoc
  where
    reader :: String -> Either String (Credential 'DelegationK)
    reader str =
       (DelegationFromKey . liftXPub <$> xpubReader allowedPrefixesForXPub str)
       `orElse`
       (DelegationFromKeyHash <$> keyhashReader (Delegation, allowedPrefixesForKeyHash) str)
       `orElse`
       (DelegationFromScript <$> scriptHashReader str)
       `orElse`
       Left "Couldn't parse delegation credentials. Neither a public key, a public key hash nor a script hash."

    -- TODO: Allow non-extended keys here.
    allowedPrefixesForXPub =
        [ CIP5.stake_xvk
        ]
    allowedPrefixesForKeyHash =
        [ CIP5.stake_vkh
        ]
