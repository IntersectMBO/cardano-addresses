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
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address.KeyHash
    (
      KeyHash (..)
    , KeyRole (..)
    , GovernanceType (..)
    , keyHashFromBytes
    , keyHashFromText
    , keyHashToText
    , ErrKeyHashFromText
    , prettyErrKeyHashFromText
    ) where

import Prelude

import Cardano.Address.Derivation
    ( credentialHashSize, hashCredential )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode, fromBase16 )
import Control.DeepSeq
    ( NFData )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Data.Bifunctor
    ( first )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

-- | Determines if one asks for deprecated HRP prefixes, '*_vkh' and '*_script'
-- in accordance to CIP-0105 (on demand when flag 'cip-0105' is used) or uses default format
-- specified in CIP-0129 (where additional byte is prepended to 28-byte hash).
data GovernanceType = CIP0129 | CIP0105
    deriving (Eq, Show)

data KeyRole =
      PaymentShared
    | DelegationShared
    | Payment
    | Delegation
    | Policy
    | Representative
    | CommitteeCold
    | CommitteeHot
    | Unknown
    deriving (Generic, Show, Ord, Eq)
instance NFData KeyRole

-- | A 'KeyHash' type represents verification key hash that participate in building
-- multi-signature script. The hash is expected to have size of 28-byte.
--
-- @since 3.0.0
data KeyHash = KeyHash
    { role :: KeyRole
    , digest :: ByteString }
    deriving (Generic, Show, Ord, Eq)
instance NFData KeyHash

-- | Construct an 'KeyHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
keyHashFromBytes :: (KeyRole, ByteString) -> Maybe KeyHash
keyHashFromBytes (cred, bytes)
    | BS.length bytes /= credentialHashSize = Nothing
    | otherwise = Just $ KeyHash cred bytes

-- | Encode a 'KeyHash' to bech32 'Text' or hex is key role unknown.
--  If one wants to include, valid in governance roles only, additional byte
--  as specified in CIP-0129, the function needs to be called with withByte=true.
--
-- @since 3.0.0
keyHashToText :: KeyHash -> Maybe GovernanceType -> Text
keyHashToText (KeyHash cred keyHash) govType = case cred of
    PaymentShared ->
        T.decodeUtf8 $ encode (EBech32 CIP5.addr_shared_vkh) keyHash
    DelegationShared ->
        T.decodeUtf8 $ encode (EBech32 CIP5.stake_shared_vkh) keyHash
    Payment ->
        T.decodeUtf8 $ encode (EBech32 CIP5.addr_vkh) keyHash
    Delegation ->
        T.decodeUtf8 $ encode (EBech32 CIP5.stake_vkh) keyHash
    Policy ->
        T.decodeUtf8 $ encode (EBech32 CIP5.policy_vkh) keyHash
    Representative -> case govType of
        Just CIP0105 ->
            T.decodeUtf8 $ encode (EBech32 CIP5.drep_vkh) keyHash
        _ ->
            T.decodeUtf8 $ encode (EBech32 CIP5.drep) $ keyHashAppendByteCIP0129 keyHash cred
    CommitteeCold -> case govType of
        Just CIP0105 ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_cold_vkh) keyHash
        _ ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_cold) $ keyHashAppendByteCIP0129 keyHash cred
    CommitteeHot -> case govType of
        Just CIP0105 ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_hot_vkh) keyHash
        _ ->
            T.decodeUtf8 $ encode (EBech32 CIP5.cc_hot) $ keyHashAppendByteCIP0129 keyHash cred
    Unknown ->
        T.decodeUtf8 $ encode EBase16 keyHash

-- | In accordance to CIP-0129 (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0129)
--   one byte is prepended to vkh only in governance context. The rules how to contruct it are summarized
--   below
--
--   drep       0010....
--   hot        0000....    key type
--   cold       0001....
--
--   keyhash    ....0010
--   This is on top of X_vkh, where X={drep, cc_hot, cc_hot}, which lacks the additional byte.
--   In `keyHashFromText` we additionally
--   support reading legacy X which also lacks the additional byte, and has the same payload as
--   as the corresponding X_vkh.
keyHashAppendByteCIP0129 :: ByteString -> KeyRole -> ByteString
keyHashAppendByteCIP0129 payload cred =
    maybe payload (`BS.cons` payload) bytePrefix
  where
    bytePrefix = case cred of
        Representative -> Just 0b00100010
        CommitteeCold -> Just 0b00010010
        CommitteeHot -> Just 0b00000010
        _ -> Nothing

-- | Construct a 'KeyHash' from 'Text'. It should be
-- Bech32 encoded text with one of following hrp:
-- - `addr_shared_vkh`
-- - `stake_shared_vkh`
-- - `addr_vkh`
-- - `stake_vkh`
-- - `policy_vkh`
-- - `drep`
-- - `cc_cold`
-- - `cc_hot`
-- - `drep_vkh`
-- - `cc_cold_vkh`
-- - `cc_hot_vkh`
-- - `addr_shared_vk`
-- - `stake_shared_vk`
-- - `addr_vk`
-- - `stake_vk`
-- - `policy_vk`
-- - `cc_cold_vk`
-- - `cc_hot_vk`
-- - `addr_shared_xvk`
-- - `stake_shared_xvk`
-- - `addr_xvk`
-- - `stake_xvk`
-- - `policy_xvk`
-- - `drep_xvk`
-- - `cc_cold_xvk`
-- - `cc_hot_xvk`
-- Raw keys will be hashed on the fly, whereas hash that are directly
-- provided will remain as such.
-- If if hex is encountered Unknown policy key is assumed
--
-- @since 3.1.0
keyHashFromText :: Text -> Either ErrKeyHashFromText KeyHash
keyHashFromText txt =
    case (fromBase16 $ T.encodeUtf8 txt) of
        Right bs ->
            if checkBSLength bs 28 then
                pure $ KeyHash Unknown bs
            else if checkBSLength bs 32 then
                pure $ KeyHash Unknown (hashCredential bs)
            else if checkBSLength bs 64 then
                pure $ KeyHash Unknown (hashCredential $ BS.take 32 bs)
            else
                Left ErrKeyHashFromTextInvalidHex
        Left _ -> do
            (hrp, dp) <- first (const ErrKeyHashFromTextInvalidString) $
                Bech32.decodeLenient txt

            maybeToRight ErrKeyHashFromTextWrongDataPart (Bech32.dataPartToBytes dp)
                >>= maybeToRight ErrKeyHashFromTextWrongHrp . convertBytes hrp
                >>= maybeToRight ErrKeyHashFromTextWrongPayload . keyHashFromBytes
 where
    convertBytes hrp bytes
        | hrp == CIP5.addr_shared_vkh && checkBSLength bytes 28 =
              Just (Payment, bytes)
        | hrp == CIP5.stake_shared_vkh && checkBSLength bytes 28 =
              Just (Delegation, bytes)
        | hrp == CIP5.addr_vkh && checkBSLength bytes 28 =
              Just (Payment, bytes)
        | hrp == CIP5.stake_vkh && checkBSLength bytes 28 =
              Just (Delegation, bytes)
        | hrp == CIP5.policy_vkh && checkBSLength bytes 28 =
              Just (Policy, bytes)
        | hrp == CIP5.drep && checkBSLength bytes 29 =
              let (fstByte, payload) = first BS.head $ BS.splitAt 1 bytes
              --   drep          0010....
              --   keyhash       ....0010
              in if fstByte == 0b00100010 then
                  Just (Representative, payload)
                 else
                  Nothing
        | hrp == CIP5.drep && checkBSLength bytes 28 =
              Just (Representative, bytes)
        | hrp == CIP5.drep_vkh && checkBSLength bytes 28 =
              Just (Representative, bytes)
        | hrp == CIP5.cc_cold && checkBSLength bytes 29 =
              let (fstByte, payload) = first BS.head $ BS.splitAt 1 bytes
              --   cold          0001....
              --   keyhash       ....0010
              in if fstByte == 0b00010010 then
                  Just (CommitteeCold, payload)
                 else
                  Nothing
        | hrp == CIP5.cc_cold && checkBSLength bytes 28 =
              Just (CommitteeCold, bytes)
        | hrp == CIP5.cc_cold_vkh && checkBSLength bytes 28 =
              Just (CommitteeCold, bytes)
        | hrp == CIP5.cc_hot && checkBSLength bytes 29 =
              let (fstByte, payload) = first BS.head $ BS.splitAt 1 bytes
              --   hot           0000....
              --   keyhash       ....0010
              in if fstByte == 0b00000010 then
                  Just (CommitteeHot, payload)
                 else
                  Nothing
        | hrp == CIP5.cc_hot && checkBSLength bytes 28 =
              Just (CommitteeHot, bytes)
        | hrp == CIP5.cc_hot_vkh && checkBSLength bytes 28 =
              Just (CommitteeHot, bytes)
        | hrp == CIP5.addr_shared_vk && checkBSLength bytes 32 =
              Just (Payment, hashCredential bytes)
        | hrp == CIP5.addr_vk && checkBSLength bytes 32 =
              Just (Payment, hashCredential bytes)
        | hrp == CIP5.addr_shared_xvk && checkBSLength bytes 64 =
              Just (Payment, hashCredential $ BS.take 32 bytes)
        | hrp == CIP5.addr_xvk && checkBSLength bytes 64 =
              Just (Payment, hashCredential $ BS.take 32 bytes)
        | hrp == CIP5.stake_shared_vk && checkBSLength bytes 32 =
              Just (Delegation, hashCredential bytes)
        | hrp == CIP5.stake_vk && checkBSLength bytes 32 =
              Just (Delegation, hashCredential bytes)
        | hrp == CIP5.stake_shared_xvk && checkBSLength bytes 64 =
              Just (Delegation, hashCredential $ BS.take 32 bytes)
        | hrp == CIP5.stake_xvk && checkBSLength bytes 64 =
              Just (Delegation, hashCredential $ BS.take 32 bytes)
        | hrp == CIP5.policy_vk && checkBSLength bytes 32 =
              Just (Policy, hashCredential bytes)
        | hrp == CIP5.policy_xvk && checkBSLength bytes 64 =
              Just (Policy, hashCredential $ BS.take 32 bytes)
        | hrp == CIP5.drep_vk && checkBSLength bytes 32 =
              Just (Representative, hashCredential bytes)
        | hrp == CIP5.drep_xvk && checkBSLength bytes 64 =
              Just (Representative, hashCredential $ BS.take 32 bytes)
        | hrp == CIP5.cc_cold_vk && checkBSLength bytes 32 =
              Just (CommitteeCold, hashCredential bytes)
        | hrp == CIP5.cc_cold_xvk && checkBSLength bytes 64 =
              Just (CommitteeCold, hashCredential $ BS.take 32 bytes)
        | hrp == CIP5.cc_hot_vk && checkBSLength bytes 32 =
              Just (CommitteeHot, hashCredential bytes)
        | hrp == CIP5.cc_hot_xvk && checkBSLength bytes 64 =
              Just (CommitteeHot, hashCredential $ BS.take 32 bytes)
        | otherwise = Nothing
    checkBSLength :: ByteString -> Int -> Bool
    checkBSLength bytes expLength =
        BS.length bytes == expLength

-- Possible errors when deserializing a key hash from text.
--
-- @since 3.0.0
data ErrKeyHashFromText
    = ErrKeyHashFromTextInvalidString
    | ErrKeyHashFromTextWrongPayload
    | ErrKeyHashFromTextWrongHrp
    | ErrKeyHashFromTextWrongDataPart
    | ErrKeyHashFromTextInvalidHex
    deriving (Show, Eq)

-- Possible errors when deserializing a key hash from text.
--
-- @since 3.0.0
prettyErrKeyHashFromText :: ErrKeyHashFromText -> String
prettyErrKeyHashFromText = \case
    ErrKeyHashFromTextInvalidString ->
        "Invalid encoded string: must be either bech32 or hex-encoded."
    ErrKeyHashFromTextWrongPayload ->
        "Verification key hash must contain exactly 28 bytes."
    ErrKeyHashFromTextWrongHrp ->
        "Invalid human-readable prefix: must be 'X_vkh', 'X_vk', 'X_xvk' where X is 'addr_shared', 'stake_shared' or 'policy'."
    ErrKeyHashFromTextWrongDataPart ->
        "Verification key hash is Bech32-encoded but has an invalid data part."
    ErrKeyHashFromTextInvalidHex ->
        "Invalid hex-encoded string: must be either 28, 32 or 64 bytes."

instance ToJSON KeyHash where
    toJSON = String . flip keyHashToText Nothing
