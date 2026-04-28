{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Vendored from @cardano-crypto@ (Apache-2.0).
-- Original: Crypto.Encoding.BIP39
--
-- Basement types replaced with standard Haskell equivalents.
module Cardano.Address.Crypto.BIP39
    ( -- * Entropy
      Entropy
    , ValidEntropySize
    , Checksum
    , ValidChecksumSize
    , MnemonicWords
    , EntropySize
    , toEntropy
    , entropyRaw
    , entropyChecksum

    , entropyToWords
    , wordsToEntropy

    , -- * Seed
      Seed
    , Passphrase
    , sentenceToSeed
    , phraseToSeed

    , -- * Mnemonic Sentence
      MnemonicSentence (..)
    , MnemonicPhrase
    , ValidMnemonicSentence
    , mnemonicPhrase
    , checkMnemonicPhrase
    , mnemonicPhraseToMnemonicSentence
    , mnemonicSentenceToMnemonicPhrase
    , mnemonicSentenceToString
    , mnemonicSentenceToListN
    , mnemonicPhraseToString
    , mnemonicPhraseToListN
    , translateTo
    , -- ** Dictionary
      Dictionary (..)
    , WordIndex
    , wordIndex
    , unWordIndex

    , -- * helpers
      ConsistentEntropy
    , CheckSumBits
    , Elem

    , -- * Errors
      DictionaryError (..)
    , EntropyError (..)
    , MnemonicWordsError (..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Data.Bits
    ( Bits (..) )
import Data.ByteArray
    ( ByteArray, ByteArrayAccess )
import Data.ByteString
    ( ByteString )
import Data.Kind
    ( Constraint )
import Data.List
    ( intersperse, reverse )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word8 )
import GHC.TypeLits
    ( ErrorMessage (..), KnownNat, Nat, TypeError, natVal )

import Cardano.Address.Crypto.ListN
    ( ListN )
import qualified Cardano.Address.Crypto.ListN as ListN

import qualified Crypto.Hash as Hash
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Crypto.Number.Serialize
    ( i2ospOf_, os2ip )

import Cardano.Address.Crypto.BIP39.Dictionary
    ( Dictionary (..)
    , DictionaryError (..)
    , WordIndex
    , unWordIndex
    , wordIndex
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- -------------------------------------------------------------------------- --
-- Entropy
-- -------------------------------------------------------------------------- --

newtype Checksum (bits :: Nat) = Checksum Word8
    deriving (Show, Eq)

instance NFData (Checksum bits) where
    rnf (Checksum !_) = ()

checksum
    :: forall csz ba
     . (KnownNat csz, ByteArrayAccess ba)
    => ba -> Checksum csz
checksum bs =
    Checksum
        $ (Hash.hashWith Hash.SHA256 bs `BA.index` 0)
            `shiftR` (8 - csz)
  where
    csz = fromInteger $ natVal (Proxy @csz)

type ValidChecksumSize (ent :: Nat) (csz :: Nat) =
    ( KnownNat csz
    , Elem csz '[3, 4, 5, 6, 7, 8]
    , CheckSumBits ent ~ csz
    )

type family CheckSumBits (n :: Nat) :: Nat where
    CheckSumBits 96 = 3
    CheckSumBits 128 = 4
    CheckSumBits 160 = 5
    CheckSumBits 192 = 6
    CheckSumBits 224 = 7
    CheckSumBits 256 = 8

data Entropy (n :: Nat) = Entropy
    { entropyRaw :: !ByteString
    , entropyChecksum :: !(Checksum (CheckSumBits n))
    }
    deriving (Show, Eq)

instance NFData (Entropy n) where
    rnf (Entropy !_ cs) = rnf cs

type ValidEntropySize (n :: Nat) =
    ( KnownNat n
    , Elem n '[96, 128, 160, 192, 224, 256]
    )

toEntropy
    :: forall n csz ba
     . (ValidEntropySize n, ValidChecksumSize n csz, ByteArrayAccess ba)
    => ba
    -> Either (EntropyError csz) (Entropy n)
toEntropy bs
    | actual == expected =
        Right $ Entropy (BA.convert bs) (checksum @csz bs)
    | otherwise =
        Left $ ErrInvalidEntropyLength actual expected
  where
    actual = BA.length bs * 8
    expected = fromInteger $ natVal (Proxy @n)

toEntropyCheck
    :: forall n csz ba
     . (ValidEntropySize n, ValidChecksumSize n csz, ByteArrayAccess ba)
    => ba
    -> Checksum csz
    -> Either (EntropyError csz) (Entropy n)
toEntropyCheck bs s = case toEntropy bs of
    Left err -> Left err
    Right e@(Entropy _ cs)
        | cs == s -> Right e
        | otherwise -> Left $ ErrInvalidEntropyChecksum cs s

type family MnemonicWords (n :: Nat) :: Nat where
    MnemonicWords 96 = 9
    MnemonicWords 128 = 12
    MnemonicWords 160 = 15
    MnemonicWords 192 = 18
    MnemonicWords 224 = 21
    MnemonicWords 256 = 24

type family EntropySize (n :: Nat) :: Nat where
    EntropySize 9 = 96
    EntropySize 12 = 128
    EntropySize 15 = 160
    EntropySize 18 = 192
    EntropySize 21 = 224
    EntropySize 24 = 256

type ConsistentEntropy ent mw csz =
    ( ValidEntropySize ent
    , ValidChecksumSize ent csz
    , ValidMnemonicSentence mw
    , MnemonicWords ent ~ mw
    )

wordsToEntropy
    :: forall ent csz mw
     . ConsistentEntropy ent mw csz
    => MnemonicSentence mw
    -> Either (EntropyError csz) (Entropy ent)
wordsToEntropy (MnemonicSentence ms) =
    let entropy =
            ListN.foldl'
                ( \acc x ->
                    acc `shiftL` 11
                        + toInteger (unWordIndex x)
                )
                0
                ms
        initialEntropy :: ByteString
        initialEntropy =
            i2ospOf_ nb (entropy `shiftR` fromInteger checksumsize)
        cs = Checksum $ fromInteger (entropy .&. mask)
     in toEntropyCheck initialEntropy cs
  where
    checksumsize = natVal (Proxy @csz)
    entropysize = natVal (Proxy @ent)
    nb = fromInteger entropysize `div` 8
    mask = 2 ^ checksumsize - 1

entropyToWords
    :: forall n csz mw
     . ConsistentEntropy n mw csz
    => Entropy n
    -> MnemonicSentence mw
entropyToWords (Entropy bs (Checksum w)) =
    MnemonicSentence
        $ fromMaybe (error "entropyToWords: invalid length")
        $ ListN.toListN
        $ reverse
        $ loop mw g
  where
    g =
        (os2ip bs `shiftL` fromIntegral csz)
            .|. fromIntegral w
    csz = natVal (Proxy @csz)
    mw = natVal (Proxy @mw)
    loop nbWords acc
        | nbWords == 0 = []
        | otherwise =
            let (acc', d) = acc `divMod` 2048
             in wordIndex (fromIntegral d)
                    : loop (nbWords - 1) acc'

-- -------------------------------------------------------------------------- --
-- Seed
-- -------------------------------------------------------------------------- --

newtype Seed = Seed ByteString
    deriving (Show, Eq, Ord)
    deriving newtype (Semigroup, Monoid, ByteArrayAccess, ByteArray, NFData)

type Passphrase = Text

sentenceToSeed
    :: ValidMnemonicSentence mw
    => MnemonicSentence mw
    -> Dictionary
    -> Passphrase
    -> Seed
sentenceToSeed mw dic =
    phraseToSeed (mnemonicSentenceToMnemonicPhrase dic mw) dic

phraseToSeed
    :: ValidMnemonicSentence mw
    => MnemonicPhrase mw
    -> Dictionary
    -> Passphrase
    -> Seed
phraseToSeed mw dic passphrase =
    PBKDF2.fastPBKDF2_SHA512
        (PBKDF2.Parameters 2048 64)
        sentence
        (T.encodeUtf8 ("mnemonic" <> passphrase))
  where
    sentence = T.encodeUtf8 $ mnemonicPhraseToString dic mw

-- -------------------------------------------------------------------------- --
-- Mnemonic Sentence and Mnemonic Phrase
-- -------------------------------------------------------------------------- --

newtype MnemonicSentence (mw :: Nat) = MnemonicSentence
    { mnemonicSentenceToListN :: ListN mw WordIndex
    }
    deriving (Show, Eq)

instance NFData (MnemonicSentence mw) where
    rnf (MnemonicSentence l) = rnf l

type ValidMnemonicSentence (mw :: Nat) =
    ( KnownNat mw
    , Elem mw '[9, 12, 15, 18, 21, 24]
    )

newtype MnemonicPhrase (mw :: Nat) = MnemonicPhrase
    { mnemonicPhraseToListN :: ListN mw Text
    }
    deriving (Show, Eq)

instance NFData (MnemonicPhrase mw) where
    rnf (MnemonicPhrase l) = rnf l

mnemonicPhrase
    :: forall mw
     . ValidMnemonicSentence mw
    => [Text]
    -> Either MnemonicWordsError (MnemonicPhrase mw)
mnemonicPhrase l =
    MnemonicPhrase
        <$> maybe
            ( Left $
                ErrWrongNumberOfWords
                    (fromIntegral $ Prelude.length l)
                    (fromInteger $ natVal (Proxy @mw))
            )
            Right
            (ListN.toListN l)
{-# INLINEABLE mnemonicPhrase #-}

checkMnemonicPhrase
    :: forall mw
     . ValidMnemonicSentence mw
    => Dictionary
    -> MnemonicPhrase mw
    -> Bool
checkMnemonicPhrase dic (MnemonicPhrase ln) =
    ListN.foldl' (\acc s -> dictionaryTestWord dic s && acc) True ln

mnemonicPhraseToMnemonicSentence
    :: forall mw
     . ValidMnemonicSentence mw
    => Dictionary
    -> MnemonicPhrase mw
    -> Either DictionaryError (MnemonicSentence mw)
mnemonicPhraseToMnemonicSentence dic (MnemonicPhrase ln) =
    MnemonicSentence <$> ListN.mapM (dictionaryWordToIndex dic) ln

mnemonicSentenceToMnemonicPhrase
    :: forall mw
     . ValidMnemonicSentence mw
    => Dictionary
    -> MnemonicSentence mw
    -> MnemonicPhrase mw
mnemonicSentenceToMnemonicPhrase dic (MnemonicSentence ln) =
    MnemonicPhrase $ ListN.map (dictionaryIndexToWord dic) ln

mnemonicPhraseToString
    :: forall mw
     . ValidMnemonicSentence mw
    => Dictionary
    -> MnemonicPhrase mw
    -> Text
mnemonicPhraseToString dic (MnemonicPhrase ln) =
    T.intercalate
        (dictionaryWordSeparator dic)
        (ListN.unListN ln)

mnemonicSentenceToString
    :: forall mw
     . ValidMnemonicSentence mw
    => Dictionary
    -> MnemonicSentence mw
    -> Text
mnemonicSentenceToString dic =
    mnemonicPhraseToString dic
        . mnemonicSentenceToMnemonicPhrase dic

translateTo
    :: forall mw
     . ValidMnemonicSentence mw
    => Dictionary
    -> Dictionary
    -> MnemonicPhrase mw
    -> Either DictionaryError (MnemonicPhrase mw)
translateTo dicSrc dicDst (MnemonicPhrase ln) =
    MnemonicPhrase
        <$> ListN.mapM
            ( return . dictionaryIndexToWord dicDst
                <=< dictionaryWordToIndex dicSrc
            )
            ln

-- -------------------------------------------------------------------------- --
-- Helpers
-- -------------------------------------------------------------------------- --

type family Elem (e :: Nat) (l :: [Nat]) :: Constraint where
    Elem e '[] =
        TypeError
            ( 'Text "offset: field "
                ':<>: 'ShowType e
                ':<>: 'Text " not element of valid values"
            )
    Elem e (e ': _) = ()
    Elem e (_ ': xs) = Elem e xs

-- -------------------------------------------------------------------------- --
-- Errors
-- -------------------------------------------------------------------------- --

data EntropyError csz
    = ErrInvalidEntropyLength
        Int -- Actual length in bits
        Int -- Expected length in bits
    | ErrInvalidEntropyChecksum
        (Checksum csz) -- Actual checksum
        (Checksum csz) -- Expected checksum
    deriving Show

data MnemonicWordsError
    = ErrWrongNumberOfWords
        Int -- Actual number of words
        Int -- Expected number of words
    deriving Show
