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
import Cardano.Address.KeyHash
    ( KeyRole (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..), liftPub, liftXPub )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )
import Options.Applicative.Derivation
    ( keyhashReader, pubReader, xpubReader )
import Options.Applicative.Script
    ( scriptHashReader, scriptReader )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

--
-- Applicative Parser
--

delegationCredentialArg  :: String -> Parser (Credential 'DelegationK)
delegationCredentialArg helpDoc = argument (eitherReader reader) $ mempty
    <> metavar "EXTENDED KEY || NON-EXTENDED KEY || KEY HASH || SCRIPT || SCRIPT HASH"
    <> help helpDoc
  where
    reader :: String -> Either String (Credential 'DelegationK)
    reader str =
       (DelegationFromKey . liftPub <$> pubReader allowedPrefixesForPub str)
       `orElse`
       (DelegationFromExtendedKey . liftXPub <$> xpubReader allowedPrefixesForXPub str)
       `orElse`
       (DelegationFromKeyHash <$> keyhashReader (Delegation, allowedPrefixesForKeyHash) str)
       `orElse`
       (DelegationFromScriptHash <$> scriptHashReader str)
       `orElse`
       (DelegationFromScript <$> scriptReader str)
       `orElse`
       Left errMsg

    errMsg = mconcat
        [ "Couldn't parse delegation credentials. Neither an extended public key, "
        , "a non-extended public key, a public key hash, a script nor a script hash."
        ]

    allowedPrefixesForPub =
        [ CIP5.stake_vk
        ]
    allowedPrefixesForXPub =
        [ CIP5.stake_xvk
        ]
    allowedPrefixesForKeyHash =
        [ CIP5.stake_vkh
        ]
