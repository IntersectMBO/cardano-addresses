{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Cardano.Address.Script
    (
    -- * Script
      Script (..)
    , serializeScript
    , foldScript

    -- * Script template
    , ScriptTemplate (..)
    , Cosigner (..)
    , cosignerToText

    -- * Validation
    , ValidationLevel (..)
    , ErrValidateScript (..)
    , ErrRecommendedValidateScript (..)
    , ErrValidateScriptTemplate (..)
    , validateScript
    , validateScriptTemplate
    , validateScriptOfTemplate
    , prettyErrValidateScript
    , prettyErrValidateScriptTemplate

    -- * Hashing
    , ScriptHash (..)
    , toScriptHash
    , scriptHashFromBytes
    , scriptHashToText
    , scriptHashFromText
    , prettyErrScriptHashFromText

    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, credentialHashSize, hashCredential, xpubFromBytes, xpubToBytes )
import Cardano.Address.KeyHash
    ( GovernanceType (..)
    , KeyHash (..)
    , KeyRole (..)
    , keyHashFromText
    , prettyErrKeyHashFromText
    )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode, fromBase16 )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( foldM, unless, when )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Aeson.Key
    ( fromText, toText )
import Data.Aeson.Types
    ( Parser )
import Data.Bifunctor
    ( first )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Foldable
    ( asum, foldl', traverse_ )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Hashable
    ( Hashable )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( Map )
import Data.Text
    ( Text )
import Data.Traversable
    ( for )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.HashSet as Set
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

-- | A 'Script' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data Script (elem :: Type)
    = RequireSignatureOf !elem
    | RequireAllOf ![Script elem]
    | RequireAnyOf ![Script elem]
    | RequireSomeOf Word8 ![Script elem]
    | ActiveFromSlot Natural
    | ActiveUntilSlot Natural
    deriving stock (Generic, Show, Eq)
instance NFData elem => NFData (Script elem)

-- | This function realizes what cardano-node's `Api.serialiseToCBOR script` realizes
-- This is basically doing the symbolically following:
-- toCBOR [0,multisigScript]
--
-- @since 3.0.0
serializeScript :: Script KeyHash -> ByteString
serializeScript script =
    multisigTag <> CBOR.toStrictByteString (toCBOR script)
  where
    -- | Magic number representing the tag of the native multi-signature script
    -- language. For each script language included, a new tag is chosen.
    multisigTag :: ByteString
    multisigTag = "\00"

    toCBOR :: Script KeyHash -> CBOR.Encoding
    toCBOR = \case
        RequireSignatureOf (KeyHash _ verKeyHash) ->
            encodeMultiscriptCtr 0 2 <> CBOR.encodeBytes verKeyHash
        RequireAllOf contents ->
            encodeMultiscriptCtr 1 2 <> encodeFoldable toCBOR contents
        RequireAnyOf contents ->
            encodeMultiscriptCtr 2 2 <> encodeFoldable toCBOR contents
        RequireSomeOf m contents -> mconcat
            [ encodeMultiscriptCtr 3 3
            , CBOR.encodeInt (fromInteger $ toInteger m)
            , encodeFoldable toCBOR contents
            ]
        ActiveFromSlot slotNum ->
            encodeMultiscriptCtr 4 2 <> CBOR.encodeWord64 (fromInteger $ toInteger slotNum)
        ActiveUntilSlot slotNum ->
            encodeMultiscriptCtr 5 2 <> CBOR.encodeWord64 (fromInteger $ toInteger slotNum)

    encodeMultiscriptCtr :: Word -> Word -> CBOR.Encoding
    encodeMultiscriptCtr ctrIndex listLen =
        CBOR.encodeListLen listLen <> CBOR.encodeWord ctrIndex

    encodeFoldable :: (Foldable f) => (a -> CBOR.Encoding) -> f a -> CBOR.Encoding
    encodeFoldable encode' xs = wrapArray len contents
      where
        (len, contents) = foldl' go (0, mempty) xs
        go (!l, !enc) next = (l + 1, enc <> encode' next)

        wrapArray :: Word -> CBOR.Encoding -> CBOR.Encoding
        wrapArray len' contents'
            | len' <= 23 = CBOR.encodeListLen len' <> contents'
            | otherwise  = CBOR.encodeListLenIndef <> contents' <> CBOR.encodeBreak

-- | Represents the cosigner of the script, ie., party that co-shares the script.
--
-- @since 3.2.0
newtype Cosigner = Cosigner Word8
    deriving (Generic, Ord, Eq)
instance Hashable Cosigner
instance NFData Cosigner

instance Show Cosigner where
    show = T.unpack . cosignerToText

-- | Represents the script template that show the structure of the script and determines
-- the expected place of verification keys corresponding to given cosigners.
--
-- @since 3.2.0
data ScriptTemplate = ScriptTemplate
    { cosigners :: Map Cosigner XPub
    , template :: Script Cosigner
    } deriving (Generic, Show, Eq)
instance NFData ScriptTemplate

-- | Computes the hash of a given script, by first serializing it to CBOR.
--
-- @since 3.0.0
toScriptHash :: Script KeyHash -> ScriptHash
toScriptHash = ScriptHash . hashCredential . serializeScript

-- | A 'ScriptHash' type represents script hash. The hash is expected to have size of
-- 28-byte.
--
-- @since 3.0.0
newtype ScriptHash = ScriptHash { unScriptHash :: ByteString }
    deriving (Generic, Show, Ord, Eq)
instance NFData ScriptHash

-- | Construct an 'ScriptHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
scriptHashFromBytes :: ByteString -> Maybe ScriptHash
scriptHashFromBytes bytes
    | BS.length bytes /= credentialHashSize = Nothing
    | otherwise = Just $ ScriptHash bytes

-- | Encode a 'ScriptHash' to bech32 'Text' or hex if key role is unknown.
-- In the case of governance role, if one wants to include additional byte
-- as specified in [CIP-0129](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0129/README.md)
-- unless the function is called with CIP0105.
--
-- One byte is prepended to script hash only in governance context. The rules how to contruct it are summarized
-- below
--
-- @
--   drep       0010....
--   hot        0000....    key type
--   cold       0001....
--
--   scripthash ....0011    credential type
-- @
--
-- This is on top of @X_script@, where @X={drep, cc_hot, cc_hot}@, which lacks the additional byte.
-- In `scriptHashFromText` we additionally
-- support reading legacy X which also lacks the additional byte, and has the same payload as
-- as the corresponding X_script.
--
-- @since 4.0.0
scriptHashToText :: ScriptHash -> KeyRole -> Maybe GovernanceType -> Text
scriptHashToText (ScriptHash scriptHash) cred govType = case cred of
    Representative -> case govType of
        Just CIP0105 ->
            T.decodeUtf8 $ encode (EBech32 CIP5.drep_script) scriptHash
        _ ->
            T.decodeUtf8 $ encode (EBech32 CIP5.drep) $ appendByte scriptHash
    CommitteeCold -> case govType of
        Just CIP0105 ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_cold_script) scriptHash
        _ ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_cold) $ appendByte scriptHash
    CommitteeHot -> case govType of
        Just CIP0105 ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_hot_script) scriptHash
        _ ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_hot) $ appendByte scriptHash
    Unknown ->
        T.decodeUtf8 $ encode EBase16 scriptHash
    _ ->
        T.decodeUtf8 $ encode (EBech32 CIP5.script) scriptHash
  where
    appendByte payload =  maybe payload (`BS.cons` payload) bytePrefix
    bytePrefix = case cred of
        Representative -> Just 0b00100011
        CommitteeCold -> Just 0b00010011
        CommitteeHot -> Just 0b00000011
        _ -> Nothing

-- | Construct a 'ScriptHash' from 'Text'. It should be
--
-- Bech32 encoded text with one of following hrp:
--
-- - `script`
-- - `drep`
-- - `cc_cold`
-- - `cc_hot`
-- - `drep_script`
-- - `cc_cold_script`
-- - `cc_hot_script`
--
-- If if hex is encountered it is converted in rawly fashion
--
-- @since 4.0.0
scriptHashFromText :: Text -> Either ErrScriptHashFromText ScriptHash
scriptHashFromText txt =
    case (fromBase16 $ T.encodeUtf8 txt) of
        Right bs ->
            if checkBSLength bs 28 then
                pure $ ScriptHash bs
            else
                Left ErrScriptHashFromTextInvalidHex
        Left _ -> do
            (hrp, dp) <- first (const ErrScriptHashFromTextInvalidString) $
                Bech32.decodeLenient txt

            maybeToRight ErrScriptHashFromTextWrongDataPart (Bech32.dataPartToBytes dp)
                >>= maybeToRight ErrScriptHashFromTextWrongHrp . convertBytes hrp
                >>= maybeToRight ErrScriptHashFromTextWrongPayload . scriptHashFromBytes
 where
    convertBytes hrp bytes
        | hrp == CIP5.drep && checkBSLength bytes 29 =
              let (fstByte, payload) = first BS.head $ BS.splitAt 1 bytes
              --   drep          0010....
              --   scripthash    ....0011
              in if fstByte == 0b00100011 then
                  Just payload
                 else
                  Nothing
        | hrp == CIP5.drep_script && checkBSLength bytes 28 =
              Just bytes
        | hrp == CIP5.cc_cold && checkBSLength bytes 29 =
              let (fstByte, payload) = first BS.head $ BS.splitAt 1 bytes
              --   cold          0001....
              --   scripthash    ....0011
              in if fstByte == 0b00010011 then
                  Just payload
                 else
                  Nothing
        | hrp == CIP5.cc_cold_script && checkBSLength bytes 28 =
              Just bytes
        | hrp == CIP5.cc_hot && checkBSLength bytes 29 =
              let (fstByte, payload) = first BS.head $ BS.splitAt 1 bytes
              --   hot           0000....
              --   scripthash    ....0011
              in if fstByte == 0b00000011 then
                  Just payload
                 else
                  Nothing
        | hrp == CIP5.cc_hot_script && checkBSLength bytes 28 =
              Just bytes
        | hrp == CIP5.script && checkBSLength bytes 28 =
              Just bytes
        | otherwise = Nothing
    checkBSLength :: ByteString -> Int -> Bool
    checkBSLength bytes expLength =
        BS.length bytes == expLength

-- | Validation level. Required level does basic check that will make sure the script
-- is accepted in ledger. Recommended level collects a number of checks that will
-- warn about dangerous, unwise and redundant things present in the script.
--
-- @since 3.2.0
data ValidationLevel = RequiredValidation | RecommendedValidation
    deriving (Show, Eq, Generic)
instance NFData ValidationLevel

-- | Possible errors when deserializing a script hash from text.
--
-- @since 4.0.0
data ErrScriptHashFromText
    = ErrScriptHashFromTextInvalidString
    | ErrScriptHashFromTextWrongPayload
    | ErrScriptHashFromTextWrongHrp
    | ErrScriptHashFromTextWrongDataPart
    | ErrScriptHashFromTextInvalidHex
    deriving (Show, Eq)

-- | Possible errors when deserializing a script hash from text.
--
-- @since 4.0.0
prettyErrScriptHashFromText :: ErrScriptHashFromText -> String
prettyErrScriptHashFromText = \case
    ErrScriptHashFromTextInvalidString ->
        "Invalid encoded string: must be either bech32 or hex-encoded."
    ErrScriptHashFromTextWrongPayload ->
        "Script hash must contain exactly 28-byte payload and one specific prepended byte."
    ErrScriptHashFromTextWrongHrp ->
        "Invalid human-readable prefix: must be 'drep', 'cc_hot' or 'cc_cold'."
    ErrScriptHashFromTextWrongDataPart ->
        "Script hash is Bech32-encoded but has an invalid data part."
    ErrScriptHashFromTextInvalidHex ->
        "Invalid hex-encoded string: must be 28 bytes."

--
-- Script folding
--

-- | 'Script' folding
--
-- @since 3.2.0
foldScript :: (a -> b -> b) -> b -> Script a -> b
foldScript fn zero = \case
    RequireSignatureOf k -> fn k zero
    RequireAllOf xs      -> foldMScripts xs
    RequireAnyOf xs      -> foldMScripts xs
    RequireSomeOf _ xs   -> foldMScripts xs
    ActiveFromSlot _     -> zero
    ActiveUntilSlot _    -> zero
  where
    foldMScripts =
        runIdentity . foldM (\acc -> Identity . foldScript fn acc) zero

--
-- Script validation
--

-- | Validate a 'Script', semantically
--
-- @since 3.0.0
validateScript
    :: ValidationLevel
    -> Script KeyHash
    -> Either ErrValidateScript ()
validateScript level script = do
    let validateKeyHash (KeyHash _ bytes) =
            (BS.length bytes == credentialHashSize)
    let allSigs = foldScript (:) [] script
    unless (L.all validateKeyHash allSigs) $ Left WrongKeyHash

    when (L.length (L.nub $ map role allSigs) > 1) $
        Left NotUniformKeyType

    requiredValidation script

    when (level == RecommendedValidation) $
        first NotRecommended (recommendedValidation script)

requiredValidation
    :: Script elem
    -> Either ErrValidateScript ()
requiredValidation script =
    unless (check script) $ Left LedgerIncompatible
  where
    check = \case
        RequireSignatureOf _ -> True

        RequireAllOf xs ->
            L.all check xs

        RequireAnyOf xs ->
            L.any check xs

        RequireSomeOf m xs ->
            m <= sum (fmap (\x -> if check x then 1 else 0) xs)

        ActiveFromSlot _ -> True

        ActiveUntilSlot _ -> True

recommendedValidation
    :: Eq elem
    => Script elem
    -> Either ErrRecommendedValidateScript ()
recommendedValidation = \case
    RequireSignatureOf _ -> pure ()

    RequireAllOf script -> do
        when (L.null (omitTimelocks script)) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        when (redundantTimelocks script) $ Left RedundantTimelocks
        when (timelockTrap script) $ Left TimelockTrap
        traverse_ recommendedValidation script

    RequireAnyOf script -> do
        when (hasDuplicate script) $ Left DuplicateSignatures
        when (redundantTimelocks script) $ Left RedundantTimelocks
        when (redundantTimelocksInAny script) $ Left RedundantTimelocks
        traverse_ recommendedValidation script

    RequireSomeOf m script -> do
        when (m == 0) $ Left MZero
        when (length (omitTimelocks script) < fromIntegral m) $ Left ListTooSmall
        when (hasDuplicate script) $ Left DuplicateSignatures
        when (redundantTimelocks script) $ Left RedundantTimelocks
        traverse_ recommendedValidation script

    ActiveFromSlot _ -> pure ()

    ActiveUntilSlot _ -> pure ()
  where
    hasDuplicate xs =
        length sigs /= length (L.nub sigs)
      where
        sigs = [ sig | RequireSignatureOf sig <- xs ]
    hasTimelocks = \case
        ActiveFromSlot _ -> True
        ActiveUntilSlot _ -> True
        _ -> False
    redundantTimelocks xs = case L.filter hasTimelocks xs of
        [] -> False
        [_] -> False
        [_, _] -> False
        _ -> True
    -- situation where any [active_until slot1, active_from slot2]
    -- (a) acceptable when slot1 < slot2 as either it is satisfied
    --    (0, slot1) or <slot2, +inf)
    -- (b) otherwise redundant as it is always satified
    redundantTimelocksInAny xs = case L.filter hasTimelocks xs of
        [] -> False
        [_] -> False
        [ActiveFromSlot s1, ActiveUntilSlot s2] -> s2 >= s1
        [ActiveUntilSlot s2, ActiveFromSlot s1] -> s2 >= s1
        _ -> True
    -- situation where all [active_until slot1, active_from slot2]
    -- (a) trap when slot1 < slot2 as both can never be satisfied
    --    (0, slot1)
    --               (slot2, +inf)
    -- (b) acceptable when slot1 == slot2
    --    then all satisfied at slot1
    -- (c) acceptable when slot1 >= slot2
    --    then all satisfied at <slot2, slot1)
    timelockTrap xs =  case L.filter hasTimelocks xs of
        [ActiveFromSlot s1, ActiveUntilSlot s2] -> s2 < s1
        [ActiveUntilSlot s2, ActiveFromSlot s1] -> s2 < s1
        _ -> False
    omitTimelocks = filter (not . hasTimelocks)
--
-- ScriptTemplate validation
--

-- | Validate a 'ScriptTemplate', semantically
--
-- @since 3.2.0
validateScriptTemplate
    :: ValidationLevel
    -> ScriptTemplate
    -> Either ErrValidateScriptTemplate ()
validateScriptTemplate level (ScriptTemplate cosigners_ script) = do
    first WrongScript (validateScriptOfTemplate level script)
    check NoCosignerInScript (nonEmpty scriptCosigners)
    check NoCosignerXPub (nonEmpty cosignerKeys)
    check DuplicateXPubs (Set.size cosignerKeys == Map.size cosigners_)
    check UnknownCosigner (cosignerSet `Set.isSubsetOf` scriptCosigners)
    check MissingCosignerXPub (scriptCosigners `Set.isSubsetOf` cosignerSet)
  where
    scriptCosigners = Set.fromList $ foldScript (:) [] script
    cosignerKeys = Set.fromList $ Map.elems cosigners_
    cosignerSet = Set.fromList $ Map.keys cosigners_

    -- throws error if condition doesn't apply
    check err cond = unless cond (Left err)
    nonEmpty = not . Set.null

-- | Validate a script in 'ScriptTemplate'
--
-- @since 3.5.0
validateScriptOfTemplate
    :: ValidationLevel
    -> Script Cosigner
    -> Either ErrValidateScript ()
validateScriptOfTemplate level script = do
    requiredValidation script
    when (level == RecommendedValidation ) $
        first NotRecommended (recommendedValidation script)

-- | Possible validation errors when validating a script
--
-- @since 3.0.0
data ErrValidateScript
    = LedgerIncompatible
    | WrongKeyHash
    | NotUniformKeyType
    | Malformed
    | NotRecommended ErrRecommendedValidateScript
    deriving (Eq, Show)

-- | Possible recommended validation errors when validating a script
--
-- @since 3.2.0
data ErrRecommendedValidateScript
    = EmptyList
    | ListTooSmall
    | MZero
    | DuplicateSignatures
    | RedundantTimelocks
    | TimelockTrap
    deriving (Eq, Show)

-- | Possible validation errors when validating a script template
--
-- @since 3.2.0
data ErrValidateScriptTemplate
    = WrongScript ErrValidateScript
    | DuplicateXPubs
    | UnknownCosigner
    | MissingCosignerXPub
    | NoCosignerInScript
    | NoCosignerXPub
    deriving (Eq, Show)

-- | Pretty-print a script validation error.
--
-- @since 3.0.0
prettyErrValidateScript
    :: ErrValidateScript
    -> String
prettyErrValidateScript = \case
    LedgerIncompatible ->
        "The script is ill-formed and is not going to be accepted by the ledger."
    WrongKeyHash ->
        "The hash of verification key is expected to have "
        <> show credentialHashSize <> " bytes."
    NotUniformKeyType ->
        "All keys of a script must have the same role: payment, delegation, policy, \
        \representative, committee cold or committee hot."
    Malformed ->
        "Parsing of the script failed. The script should be composed of nested \
        \lists, the verification keys should be bech32-encoded with prefix \
        \'X_vkh', 'X_vk', 'X_xvk' where X is 'addr_shared', 'stake_shared', 'policy', \
        \'drep', 'cc_cold' or 'cc_hot' and timelocks must use non-negative \
        \numbers as slots."
    NotRecommended EmptyList ->
        "The list inside a script is empty or only contains timelocks \
        \(which is not recommended)."
    NotRecommended MZero ->
        "At least's coefficient is 0 (which is not recommended)."
    NotRecommended ListTooSmall ->
        "At least's coefficient is larger than the number of non-timelock \
        \elements in the list (which is not recommended)."
    NotRecommended DuplicateSignatures ->
        "The list inside a script has duplicate keys (which is not recommended)."
    NotRecommended RedundantTimelocks ->
        "Some timelocks used are redundant (which is not recommended)."
    NotRecommended TimelockTrap ->
        "The timelocks used are contradictory when used with 'all' (which is not recommended)."

-- | Pretty-print a script template validation error.
--
-- @since 3.2.0
prettyErrValidateScriptTemplate
    :: ErrValidateScriptTemplate
    -> String
prettyErrValidateScriptTemplate = \case
    WrongScript err -> prettyErrValidateScript err
    DuplicateXPubs ->
        "The cosigners in a script template must stand behind an unique extended public key."
    MissingCosignerXPub ->
        "Each cosigner in a script template must have an extended public key."
    NoCosignerInScript ->
        "The script of a template must have at least one cosigner defined."
    NoCosignerXPub ->
        "The script template must have at least one cosigner with an extended public key."
    UnknownCosigner ->
        "The specified cosigner must be present in the script of the template."
--
-- Internal
--

    -- Examples of Script jsons:
--"addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
--"stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5"
--{ "all" : [ "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
--          , "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
--          ]
--}
--{ "all" : [ "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
--          , {"any": [ "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
--                    , "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
--                    ]
--            }
--          ]
--}
--{ "all" : [ "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
--          , {"some": { "from" :[ "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
--                               , "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
--                               , "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
--                               ]
--                     , "at_least" : 2
--                     }
--            }
--          ]
--}
--{ "all" : [ "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
--          , {"active_from": 120 }
--          ]
--}
--{ "all" : [ "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
--          , any [{"active_until": 100 }, {"active_from": 120 }]
--          ]
--}

instance ToJSON elem => ToJSON (Script elem) where
    toJSON (RequireSignatureOf content) = toJSON content
    toJSON (RequireAllOf content) =
        object ["all" .= fmap toJSON content]
    toJSON (RequireAnyOf content) =
        object ["any" .= fmap toJSON content]
    toJSON (RequireSomeOf count scripts) =
        object ["some" .= object ["at_least" .= count, "from" .= scripts]]
    toJSON (ActiveFromSlot slot) =
        object ["active_from" .= slot]
    toJSON (ActiveUntilSlot slot) =
        object ["active_until" .= slot]

instance FromJSON (Script KeyHash) where
    parseJSON v =
        fromScriptJson parseKey backtrack v
      where
        parseKey = withText "Script KeyHash" $
            either
                (fail . prettyErrKeyHashFromText)
                (pure . RequireSignatureOf)
                . keyHashFromText

        -- NOTE: Because we use an alternative sum to define all parsers, in
        -- case all parser fails, only the last error is returned which can be
        -- very misleading. For example, sending {"any": []} yields an error
        -- telling us that the key `"some"` is missing.
        --
        -- To cope with this, we add a last parser 'backtrack' which always
        -- fail but with a more helpful error which tries its best at
        -- identifying the right constructor.
        backtrack = \case
            Object o -> do
                mAny  <- o .:? "any"  :: Parser (Maybe Value)
                mAll  <- o .:? "all"  :: Parser (Maybe Value)
                mSome <- o .:? "some" :: Parser (Maybe Value)
                case (mAny, mAll, mSome) of
                    (Just{}, Nothing, Nothing)  -> parseAnyOf v
                    (Nothing, Just{}, Nothing)  -> parseAllOf v
                    (Nothing, Nothing, Just{})  -> parseAtLeast v
                    (Nothing, Nothing, Nothing) -> fail
                        "Found object with unknown key. Expecting 'any', 'all' or 'some'"
                    (      _,       _,      _)  -> fail
                        "Found multiple keys 'any', 'all' and/or 'some' at the same level"
            String{} ->
                parseKey v
            _ ->
                Json.typeMismatch "Object or String" v

fromScriptJson
    :: FromJSON (Script elem)
    => (Value -> Parser (Script elem))
    -> (Value -> Parser (Script elem))
    -> Value
    -> Parser (Script elem)
fromScriptJson parseElem backtrack v =
    asum
        [ parseElem v
        , parseAnyOf v
        , parseAllOf v
        , parseAtLeast v
        , parseActiveFrom v
        , parseActiveUntil v
        ] <|> backtrack v

parseAnyOf
    :: FromJSON (Script elem)
    => Value
    -> Parser (Script elem)
parseAnyOf = withObject "Script AnyOf" $ \o ->
    RequireAnyOf <$> o .: "any"

parseAllOf
    :: FromJSON (Script elem)
    => Value
    -> Parser (Script elem)
parseAllOf = withObject "Script AllOf" $ \o ->
    RequireAllOf <$> o .: "all"

parseAtLeast
    :: FromJSON (Script elem)
    => Value
    -> Parser (Script elem)
parseAtLeast = withObject "Script SomeOf" $ \o -> do
    some <- o .: "some"
    RequireSomeOf <$> some .: "at_least" <*> some .: "from"

parseActiveFrom
    :: Value
    -> Parser (Script elem)
parseActiveFrom = withObject "Script ActiveFrom" $ \o ->
    ActiveFromSlot <$> o .: "active_from"

parseActiveUntil
    :: Value
    -> Parser (Script elem)
parseActiveUntil = withObject "Script ActiveUntil" $ \o ->
    ActiveUntilSlot <$> o .: "active_until"

cosignerToText :: Cosigner -> Text
cosignerToText (Cosigner ix) = "cosigner#"<> T.pack (show ix)

instance ToJSON Cosigner where
    toJSON = String . cosignerToText

instance FromJSON Cosigner where
    parseJSON = withText "Cosigner" $ \txt -> case T.splitOn "cosigner#" txt of
        ["",numTxt] ->  case T.decimal numTxt of
            Right (num,"") -> do
                when (num < minBound @Word8 || num > maxBound @Word8) $
                        fail "Cosigner number should be between '0' and '255'"
                pure $ Cosigner num
            _ -> fail "Cosigner should be enumerated with number"
        _ -> fail "Cosigner should be of the form: cosigner#num"

encodeXPub :: XPub -> Value
encodeXPub = String . T.decodeUtf8 . encode EBase16 . xpubToBytes

parseXPub :: Value -> Parser XPub
parseXPub = withText "XPub" $ \txt ->
    case fromBase16 (T.encodeUtf8 txt) of
        Left err -> fail err
        Right hex -> case xpubFromBytes hex of
            Nothing -> fail "Extended public key cannot be retrieved from a given hex bytestring"
            Just validXPub -> pure validXPub

instance ToJSON ScriptTemplate where
    toJSON (ScriptTemplate cosigners' template') =
        object [ "cosigners" .= object (fmap (first fromText . toPair) (Map.toList cosigners'))
               , "template" .= toJSON template']
      where
        toPair (cosigner', xpub) =
            ( cosignerToText cosigner'
            , encodeXPub xpub )

instance FromJSON (Script Cosigner) where
    parseJSON v = fromScriptJson parserCosigner backtrack v
      where
        parserCosigner o = do
            cosigner <- parseJSON @Cosigner o
            pure $ RequireSignatureOf cosigner
        backtrack = \case
            Object o -> do
                mAny  <- o .:? "any"  :: Parser (Maybe Value)
                mAll  <- o .:? "all"  :: Parser (Maybe Value)
                mSome <- o .:? "some" :: Parser (Maybe Value)
                mCos <- o .:? "cosigner" :: Parser (Maybe Value)
                case (mAny, mAll, mSome, mCos) of
                    (Just{}, Nothing, Nothing, Nothing)  -> parseAnyOf v
                    (Nothing, Just{}, Nothing, Nothing)  -> parseAllOf v
                    (Nothing, Nothing, Just{}, Nothing)  -> parseAtLeast v
                    (Nothing, Nothing, Nothing, Just{})  -> parserCosigner v
                    (Nothing, Nothing, Nothing, Nothing) -> fail
                        "Found object with unknown key. Expecting 'any', 'all', 'some' or 'cosigner'"
                    (      _,       _,      _,       _)  -> fail
                        "Found multiple keys 'any', 'all', 'cosigner' and/or 'some' at the same level"
            _ ->
                Json.typeMismatch "Object only" v

instance FromJSON ScriptTemplate where
    parseJSON = withObject "ScriptTemplate" $ \o -> do
        template' <- parseJSON <$> o .: "template"
        cosigners' <- parseCosignerPairs <$> o .: "cosigners"
        ScriptTemplate . Map.fromList <$> cosigners' <*> template'
      where
        parseCosignerPairs = withObject "Cosigner pairs" $ \o ->
            case KeyMap.toList o of
                [] -> fail "Cosigners object array should not be empty"
                cs -> for (reverse cs) $ \(numTxt, str) -> do
                    cosigner' <- parseJSON @Cosigner (String (toText numTxt))
                    xpub <- parseXPub str
                    pure (cosigner', xpub)
