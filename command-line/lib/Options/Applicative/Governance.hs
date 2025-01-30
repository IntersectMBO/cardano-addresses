{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Options.Applicative.Governance
    (
    -- * Applicative Parser
      governanceOpt
    ) where

import Prelude

import Cardano.Address.KeyHash
    ( GovernanceType (..) )
import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, flag', long )

--
-- Applicative Parser
--

-- | Parse an 'GovernanceType' from the command-line, if there is proper flag then 'cip-0105' is set.
governanceOpt :: Parser GovernanceType
governanceOpt = deprecated <|> pure CIP0129
  where
    deprecated = flag' CIP0105 (long "cip-0105")
