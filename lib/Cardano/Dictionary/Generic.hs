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

import Prelude

import Cardano.Address.Crypto.BIP39.Dictionary
    ( Dictionary (..)
    , DictionaryError (..)
    , WordIndex
    , unWordIndex
    , wordIndex
    )
import Data.Text
    ( Text )
import Data.Word
    ( Word16 )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V


toDictionary :: [Text] -> Dictionary
toDictionary wordList = Dictionary
    { dictionaryWordToIndex =
          \word -> case Map.lookup word wordMap of
                     Just x -> Right x
                     Nothing -> Left $ ErrInvalidDictionaryWord word
    , dictionaryTestWord = (`Map.member` wordMap)
    , dictionaryIndexToWord = \ix -> wordsVec V.! fromIntegral (unWordIndex ix)
    , dictionaryWordSeparator = " "
    }
  where
    wordMap :: Map.Map Text WordIndex
    wordMap =
        Map.fromList
            $ L.zip wordList
            $ L.map (wordIndex . fromIntegral) [0 :: Word16 .. 2047]

    wordsVec :: V.Vector Text
    wordsVec = V.fromList wordList
