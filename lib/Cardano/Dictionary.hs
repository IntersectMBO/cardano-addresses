{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2025 Intersect
-- License: Apache-2.0

module Cardano.Dictionary
    (
    -- * Type
     SupportedDictionary (..)

    -- * Helper
    , dictionaryFromLanguage

    ) where

import Prelude

import Cardano.Dictionary.English
    ( english )
import Cardano.Dictionary.French
    ( french )
import Cardano.Dictionary.Italian
    ( italian )
import Crypto.Encoding.BIP39.Dictionary
    ( Dictionary
    )
import GHC.Generics
    ( Generic )


data SupportedDictionary =
      English
    | Italian
    | French
    deriving (Generic, Show, Bounded, Enum, Eq)

dictionaryFromLanguage :: SupportedDictionary -> Dictionary
dictionaryFromLanguage = \case
    English -> english
    Italian -> italian
    French -> french
