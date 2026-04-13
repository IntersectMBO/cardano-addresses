{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Vendored from @cardano-crypto@ (Apache-2.0).
-- Original: Crypto.Encoding.BIP39.Dictionary
--
-- Basement types replaced:
--   Offset String -> Word16
--   NormalForm    -> NFData
--   TryFrom      -> Maybe-returning function
module Cardano.Address.Crypto.BIP39.Dictionary
    ( -- ** Dictionary
      Dictionary (..)
    , WordIndex
    , wordIndex
    , unWordIndex

    , DictionaryError (..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Data.Data
    ( Typeable )
import Data.Text
    ( Text )
import Data.Word
    ( Word16 )

-- | Describes the properties of a BIP39 word dictionary.
data Dictionary = Dictionary
    { dictionaryIndexToWord :: WordIndex -> Text
    , dictionaryWordToIndex :: Text -> Either DictionaryError WordIndex
    , dictionaryTestWord :: Text -> Bool
    , dictionaryWordSeparator :: Text
    }
    deriving Typeable

-- | Index of a mnemonic word in the 'Dictionary'.
--
-- 'WordIndex' values are in the range @[0..2047]@.
newtype WordIndex = WordIndex { _unWordIndex :: Word16 }
    deriving stock (Show, Eq, Ord, Typeable)
    deriving newtype NFData

-- | Extract the raw index.
unWordIndex :: WordIndex -> Word16
unWordIndex (WordIndex w) = w

instance Enum WordIndex where
    toEnum i = wordIndex (fromIntegral i)
    fromEnum (WordIndex w) = fromIntegral w
    succ (WordIndex v)
        | v < 2047 = WordIndex (succ v)
        | otherwise = error "WordIndex out of bound"
    pred (WordIndex v)
        | v <= 0 = error "WordIndex out of bound"
        | otherwise = WordIndex (pred v)

instance Bounded WordIndex where
    minBound = WordIndex 0
    maxBound = WordIndex 2047

-- | Smart constructor: create a 'WordIndex' from a 'Word16',
-- returning 'Nothing' if out of range.
tryWordIndex :: Word16 -> Maybe WordIndex
tryWordIndex w
    | w < 2048 = Just (WordIndex w)
    | otherwise = Nothing

-- | Create a 'WordIndex', erroring if out of range.
wordIndex :: Word16 -> WordIndex
wordIndex w = case tryWordIndex w of
    Nothing ->
        error
            ( "Error: word index should be between 0 to 2047. "
                <> show w
            )
    Just wi -> wi

-- | Errors from dictionary lookup.
newtype DictionaryError
    = ErrInvalidDictionaryWord Text
    deriving Show

instance NFData DictionaryError where
    rnf (ErrInvalidDictionaryWord t) = rnf t
