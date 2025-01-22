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


-- | Determines if one asks for deprecated HRP prefixes, '*_vkh' and '*_script'
-- in accordance to CIP-0105 (on demand when flag 'cip-0105' is used) or uses default format
-- specified in CIP-0129 (where additional byte is prepended to 28-byte hash).
data GovernanceType = CIP0129 | CIP0105
    deriving (Eq, Show)

--
-- Applicative Parser
--

-- | Parse an 'GovernanceType' from the command-line, if there is proper flag then 'cip-0105' is set.
governanceOpt :: Parser GovernanceType
governanceOpt = deprecated <|> pure CIP0129
  where
    deprecated = flag' CIP0105 (long "cip-0105")
