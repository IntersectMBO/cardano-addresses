{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune #-}

module Cardano.Mnemonic
    (
      -- * Introduction
      -- $introduction

      -- * @SomeMnemonic@
      SomeMnemonic(..)
    , mkSomeMnemonic
    , MkSomeMnemonicError(..)

      -- * @Mnemonic@
    , Mnemonic
    , mkMnemonic
    , MkMnemonicError(..)
    , mnemonicToText
    , mnemonicToEntropy

      -- * @Entropy@
    , Entropy
    , genEntropy
    , mkEntropy
    , entropyToBytes
    , entropyToMnemonic

      -- Internals & Re-export from @Crypto.Encoding.BIP39@
    , EntropyError(..)
    , DictionaryError(..)
    , MnemonicWordsError(..)
    , ValidEntropySize
    , ValidChecksumSize
    , ValidMnemonicSentence
    , ConsistentEntropy
    , CheckSumBits
    , EntropySize
    , MnemonicWords
    , MnemonicException(..)

      -- * Troubleshooting
      -- $troubleshooting
    ) where

import Prelude

import Basement.Sized.List
    ( unListN )
import Control.Arrow
    ( left )
import Control.Monad.Catch
    ( throwM )
import Crypto.Encoding.BIP39
    ( CheckSumBits
    , ConsistentEntropy
    , DictionaryError (..)
    , Entropy
    , EntropyError (..)
    , EntropySize
    , MnemonicSentence
    , MnemonicWords
    , MnemonicWordsError (..)
    , ValidChecksumSize
    , ValidEntropySize
    , ValidMnemonicSentence
    , dictionaryIndexToWord
    , entropyRaw
    , entropyToWords
    , mnemonicPhrase
    , mnemonicPhraseToMnemonicSentence
    , mnemonicSentenceToListN
    , toEntropy
    , wordsToEntropy
    )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.List
    ( intercalate )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Typeable
    ( Typeable )
import Fmt
    ( Buildable (..) )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )
import Type.Reflection
    ( typeOf )

import qualified Basement.Compat.Base as Basement
import qualified Basement.String as Basement
import qualified Crypto.Encoding.BIP39.English as Dictionary
import qualified Crypto.Random.Entropy as Crypto
import qualified Data.ByteArray as BA
import qualified Data.Text as T


-- $introduction
--
-- We call 'Entropy' an arbitrary sequence of bytes that has been generated through __high quality randomness methods__.
-- The allowed size of an 'Entropy' is @96-256@ bits and is __necessarily a multiple of 32 bits__ (4 bytes).
--
-- We call 'Mnemonic' an 'Entropy' with an appended checksum calculated by taking the first @ent / 32@ bits of the /SHA256/ hash of it,
-- where ent designates the 'Entropy' size in bits.
--
-- The concatenated result is split into groups of @11@ bits, each encoding a number from 0 to 2047 serving as an index into a [known dictionary](https://github.com/input-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt).
-- This makes for a __human-readable sentence__ of English words.
--
-- +---------------------+---------------+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
-- | Entropy Size        | Checksum Size | Sentence Length | Example                                                                                                                                         |
-- +=====================+===============+=================+=================================================================================================================================================+
-- | 96  bits (12 bytes) | 3 bits        | 9 words         | test child burst immense armed parrot company walk dog                                                                                          |
-- +---------------------+---------------+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
-- | 128 bits (16 bytes) | 4 bits        | 12 words        | test walk nut penalty hip pave soap entry language right filter choice                                                                          |
-- +---------------------+---------------+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
-- | 160 bits (20 bytes) | 5 bits        | 15 words        | art forum devote street sure rather head chuckle guard poverty release quote oak craft enemy                                                    |
-- +---------------------+---------------+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
-- | 192 bits (24 bytes) | 6 bits        | 18 words        | churn shaft spoon second erode useless thrive burst group seed element sign scrub buffalo jelly grace neck useless                              |
-- +---------------------+---------------+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
-- | 224 bits (28 bytes) | 7 bits        | 21 words        | draft ability female child jump maid roof hurt below live topple paper exclude ordinary coach churn sunset emerge blame ketchup much            |
-- +---------------------+---------------+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------+
-- | 256 bits (32 bytes) | 8 bits        | 24 words        | excess behave track soul table wear ocean cash stay nature item turtle palm soccer lunch horror start stumble month panic right must lock dress |
-- +---------------------+---------------+-----------------+-------------------------------------------------------------------------------------------------------------------------------------------------+

-- A opaque 'Mnemonic' type.
data Mnemonic (mw :: Nat) = Mnemonic
    { mnemonicToEntropy  :: Entropy (EntropySize mw)
        -- ^ Convert a 'Mnemonic' back to an 'Entropy'.
        --
        -- @since 1.0.0
    , mnemonicToSentence :: MnemonicSentence mw
    } deriving (Eq, Show)

-- This wraps EntropyError of "Cardano.Encoding.BIP39"
newtype MnemonicException csz =
    UnexpectedEntropyError (EntropyError csz)
    -- ^ Invalid entropy length or checksum
    deriving (Show, Typeable)

-- | This wraps errors from "Cardano.Encoding.BIP39"
data MkMnemonicError csz
    = ErrMnemonicWords MnemonicWordsError
      -- ^ Wrong number of words in mnemonic.
    | ErrEntropy (EntropyError csz)
      -- ^ Invalid entropy length or checksum.
    | ErrDictionary DictionaryError
      -- ^ Invalid word in mnemonic.
    deriving (Eq, Show)

deriving instance Eq (EntropyError czs)
deriving instance Eq MnemonicWordsError
deriving instance Eq DictionaryError

-- | Smart-constructor for the 'Entropy'. Make sure the 'ByteString' comes from a highly random source or use 'genEntropy'.
--
-- __example__:
--
-- >>> mkEntropy @160 bytes
-- Entropy {} :: Entropy 160
--
-- __property__:
--
-- prop> mkEntropy (entropyToBytes ent) == Right ent
--
-- @since 1.0.0
mkEntropy
    :: forall (ent :: Nat) csz. (ValidEntropySize ent, ValidChecksumSize ent csz)
    => ScrubbedBytes
    -> Either (EntropyError csz) (Entropy ent)
mkEntropy = toEntropy

-- | Generate Entropy of a given size using a cryptographically secure random seed.
--
-- __example:__
--
-- >>> genEntropy @128
-- Entropy {} :: Entropy 128
--
-- @since 1.0.0
genEntropy
    :: forall (ent :: Nat) csz. (ValidEntropySize ent, ValidChecksumSize ent csz)
    => IO (Entropy ent)
genEntropy =
    let
        size =
            fromIntegral $ natVal @ent Proxy
        eitherToIO =
            either (throwM . UnexpectedEntropyError) return
    in
        (eitherToIO . mkEntropy) =<< Crypto.getEntropy (size `div` 8)

-- | Smart-constructor for 'Mnemonic'. Requires a type application to
-- disambiguate the mnemonic size.
--
-- __example__:
--
-- >>> mkMnemonic @15 sentence
-- Mnemonic {} :: Mnemonic 15
--
-- __property__:
--
-- prop> mkMnemonic (mnemonicToText mnemonic) == Right mnemonic
--
-- @since 1.0.0
mkMnemonic
    :: forall (mw :: Nat) (ent :: Nat) csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => [Text]
    -> Either (MkMnemonicError csz) (Mnemonic mw)
mkMnemonic wordsm = do
    phrase <- left ErrMnemonicWords
        $ mnemonicPhrase @mw (toUtf8String <$> wordsm)

    sentence <- left ErrDictionary
        $ mnemonicPhraseToMnemonicSentence Dictionary.english phrase

    entropy <- left ErrEntropy
        $ wordsToEntropy sentence

    pure Mnemonic
        { mnemonicToEntropy  = entropy
        , mnemonicToSentence = sentence
        }

-- | Convert an Entropy to a corresponding Mnemonic Sentence. Since 'Entropy'
-- and 'Mnemonic' can only be created through smart-constructors, this function
-- cannot fail and is total.
--
-- @since 1.0.0
entropyToMnemonic
    :: forall mw ent csz.
     ( ValidMnemonicSentence mw
     , ValidEntropySize ent
     , ValidChecksumSize ent csz
     , ent ~ EntropySize mw
     , mw ~ MnemonicWords ent
     )
    => Entropy ent
    -> Mnemonic mw
entropyToMnemonic entropy = Mnemonic
    { mnemonicToSentence = entropyToWords entropy
    , mnemonicToEntropy  = entropy
    }

-- | Convert 'Entropy' to plain bytes.
--
-- @since 1.0.0
entropyToBytes
    :: Entropy n
    -> ScrubbedBytes
entropyToBytes = BA.convert . entropyRaw

toUtf8String
    :: Text
    -> Basement.String
toUtf8String = Basement.fromString . T.unpack

fromUtf8String
    :: Basement.String
    -> Text
fromUtf8String = T.pack . Basement.toList

instance (KnownNat csz) => Basement.Exception (MnemonicException csz)

-- | Convert a 'Mnemonic' to a sentence of English mnemonic words.
--
-- @since 1.0.0
mnemonicToText
    :: Mnemonic mw
    -> [Text]
mnemonicToText =
    map (fromUtf8String . dictionaryIndexToWord Dictionary.english)
    . unListN
    . mnemonicSentenceToListN
    . mnemonicToSentence

-- | Ease the manipulation of 'Mnemonic' by encapsulating the type constraints inside a constructor.
-- This is particularly useful for functions which do not require anything but a valid 'Mnemonic' without any
-- particular pre-condition on the size of the 'Mnemonic' itself.
--
-- @since 1.0.0
data SomeMnemonic where
    SomeMnemonic :: forall mw. KnownNat mw => Mnemonic mw -> SomeMnemonic

deriving instance Show SomeMnemonic
instance Eq SomeMnemonic where
    (SomeMnemonic mwa) == (SomeMnemonic mwb) =
        case typeOf mwa `testEquality` typeOf mwb of
            Nothing -> False
            Just Refl -> mwa == mwb

-- | This class enables caller to parse text list of variable length
-- into mnemonic sentences.
--
-- Note that the given 'Nat's **have** to be valid mnemonic sizes, otherwise the
-- underlying code won't even compile, with not-so-friendly error messages.
class MkSomeMnemonic (sz :: [Nat]) where
    -- | Construct a mnemonic from a list of words. This function is particularly useful when the
    -- number of words is not necessarily known at runtime. The function is however /ambiguous/ and
    -- requires thereby a type application.
    --
    -- __examples:__
    --
    -- >>> mkSomeMnemonic @'[ 12 ] [ "test", "child", "burst", "immense", "armed", "parrot", "company", "walk", "dog" ]
    -- Left "Invalid number of words: 12 words are expected."
    --
    -- >>> mkSomeMnemonic @'[ 9, 12, 15 ] [ "test", "child", "burst", "immense", "armed", "parrot", "company", "walk", "dog" ]
    -- Right (SomeMnemonic ...)
    --
    -- @since 1.0.0
    mkSomeMnemonic :: [Text] -> Either (MkSomeMnemonicError sz) SomeMnemonic

-- | Error reported from trying to create a passphrase from a given mnemonic
--
-- @since 1.0.0
newtype MkSomeMnemonicError (sz :: [Nat]) =
    MkSomeMnemonicError { getMkSomeMnemonicError :: String }
    deriving stock (Eq, Show)
    deriving newtype Buildable

instance {-# OVERLAPS #-}
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    , MkSomeMnemonic rest
    , NatVals rest
    ) =>
    MkSomeMnemonic (mw ': rest)
  where
    mkSomeMnemonic parts = case parseMW of
        Left err -> left (promote err) parseRest
        Right mw -> Right mw
      where
        parseMW = left (MkSomeMnemonicError . getMkSomeMnemonicError) $ -- coerce
            mkSomeMnemonic @'[mw] parts
        parseRest = left (MkSomeMnemonicError . getMkSomeMnemonicError) $ -- coerce
            mkSomeMnemonic @rest parts
        promote e e' =
            let
                sz = fromEnum <$> natVals (Proxy :: Proxy (mw ': rest))
                mw = fromEnum $ natVal (Proxy :: Proxy mw)
            in if length parts `notElem` sz
                then MkSomeMnemonicError
                    $  "Invalid number of words: "
                    <> intercalate ", " (show <$> init sz)
                    <> (if length sz > 1 then " or " else "") <> show (last sz)
                    <> " words are expected."
                else if length parts == mw then e else e'

-- | Small helper to collect 'Nat' values from a type-level list
class NatVals (ns :: [Nat]) where
    natVals :: Proxy ns -> [Integer]

instance NatVals '[] where
    natVals _ = []

instance (KnownNat n, NatVals rest) => NatVals (n ': rest) where
    natVals _ = natVal (Proxy :: Proxy n) : natVals (Proxy :: Proxy rest)

instance
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    ) =>
    MkSomeMnemonic (mw ': '[])
  where
    mkSomeMnemonic parts = do
        bimap (MkSomeMnemonicError . pretty) SomeMnemonic (mkMnemonic @mw parts)
      where
        pretty = \case
            ErrMnemonicWords ErrWrongNumberOfWords{} ->
                "Invalid number of words: "
                <> show (natVal (Proxy :: Proxy mw))
                <> " words are expected."
            ErrDictionary (ErrInvalidDictionaryWord _) ->
                "Found an unknown word not present in the pre-defined dictionary. \
                \The full dictionary is available here: \
                \https://github.com/input-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"
            ErrEntropy ErrInvalidEntropyChecksum{} ->
                "Invalid entropy checksum: please double-check the last word of \
                \your mnemonic sentence."
            ErrEntropy ErrInvalidEntropyLength{} ->
                "Something went wrong when trying to generate the entropy from \
                \the given mnemonic. As a user, there's nothing you can do."

-- $troubleshooting
--
-- - /Natural XX is out of bounds for Int/:
--   This usually occurs when ones is trying to specify an invalid size for an
--   'Entropy' or 'Mnemonic'. For example:
--
--   >>> genEntropy @42
--   error:
--     • Natural CheckSumBits 42 is out of bounds for Int
--
-- - This could be the case as well when forgetting to use an adequate type application:
--
--   >>> mkEntropy mempty
--   error:
--     • Natural ent is out of bounds for Int
