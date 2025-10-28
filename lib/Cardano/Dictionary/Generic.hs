{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2025 Intersect
-- License: Apache-2.0

module Cardano.Dictionary.Generic
    (
    -- * Helper
     toDictionary
    ) where

import Basement.Imports
    ( Bounded (..), Either (..), Maybe (..), error, flip, fromList, ($), (.) )
import Basement.Sized.Vect
    ( Vect (..), index, toVect )
import Basement.String
    ( String )
import Crypto.Encoding.BIP39.Dictionary
    ( Dictionary (..), DictionaryError (..), WordIndex, unWordIndex )
import Data.Maybe
    ( fromMaybe )

import qualified Data.List as L


toDictionary :: [String] -> Dictionary
toDictionary wordList = Dictionary
    { dictionaryWordToIndex =
          \word -> case L.lookup word wordsWithIxs of
                     Just x  -> Right x
                     Nothing -> Left $ ErrInvalidDictionaryWord word
    , dictionaryTestWord = flip L.elem wordList
    , dictionaryIndexToWord = index words . unWordIndex
    , dictionaryWordSeparator = " "
    }
  where
    wordsWithIxs :: [(String, WordIndex)]
    wordsWithIxs = L.zip wordList [minBound..maxBound]

    words :: Vect 2048 String
    words = fromMaybe (error "invalid vector length") $ toVect $ fromList wordList
