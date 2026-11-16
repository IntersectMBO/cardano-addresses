{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Options.Applicative.Discovery
    ( discoveryAddressTypeOpt
    , discoveryGapOpt
    , discoveryFromOpt
    , discoveryJsonOpt
    ) where

import Prelude

import Cardano.Address.Discovery
    ( DiscoveryAddressType (..) )
import Data.Word
    ( Word32 )
import Options.Applicative
    ( Parser
    , eitherReader
    , help
    , long
    , metavar
    , option
    , switch
    , value
    )
import Text.Read
    ( readEither )

-- | Parse the address type to generate during discovery.
discoveryAddressTypeOpt :: Parser DiscoveryAddressType
discoveryAddressTypeOpt = option (eitherReader reader) $ mempty
    <> long "type"
    <> metavar "TYPE"
    <> help "Address type to discover: 'base' or 'enterprise'."
  where
    reader :: String -> Either String DiscoveryAddressType
    reader = \case
        "base" -> Right Base
        "enterprise" -> Right Enterprise
        other -> Left $
            "Unknown address type: " <> other
            <> ". Expected one of: base, enterprise."

-- | Parse the discovery gap.
discoveryGapOpt :: Parser Word32
discoveryGapOpt = option (eitherReader (readEither @Word32)) $ mempty
    <> long "gap"
    <> metavar "INT"
    <> help "Number of consecutive unused indices to discover."

-- | Parse the starting index for discovery.
discoveryFromOpt :: Parser Word32
discoveryFromOpt = option (eitherReader (readEither @Word32)) $ mempty
    <> long "from"
    <> metavar "INT"
    <> value 0
    <> help "Soft index to start discovery from (default: 0)."

-- | Flag to request JSON output instead of the default TSV.
discoveryJsonOpt :: Parser Bool
discoveryJsonOpt = switch $ mempty
    <> long "json"
    <> help "Output the result as JSON instead of TSV."
