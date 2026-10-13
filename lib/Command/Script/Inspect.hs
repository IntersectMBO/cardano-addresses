{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Script.Inspect
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.KeyHash
    ( GovernanceType (..), KeyRole (..), keyRoleToText )
import Cardano.Address.Script
    ( ScriptHash (..)
    , prettyErrScriptHashFromText
    , scriptHashFromText
    , scriptHashToText
    )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , encode
    , fromBase16
    , fromBech32
    )
import Data.Aeson
    ( (.=) )
import Data.Bifunctor
    ( first )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text, unpack )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( pretty, vsep )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( noNewline )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding as T


data Cmd = Inspect
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "inspect" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Show information about a script hash or credential"
        <> footerDoc (Just $ vsep
            [ pretty ("The script hash or credential is read from stdin." :: Text)
            , pretty ("It can be bech32- or base16-encoded." :: Text)
            , pretty ("" :: Text)
            , pretty ("Bech32 input can be a generic script hash (script) or a" :: Text)
            , pretty ("governance script credential (drep, cc_cold, cc_hot and" :: Text)
            , pretty ("their *_script legacy CIP-0105 counterparts)." :: Text)
            , pretty ("" :: Text)
            , pretty ("Example:" :: Text)
            , pretty ("$ cardano-address script hash \"all []\" \\" :: Text)
            , pretty ("    | cardano-address script inspect" :: Text)
            , pretty ("{" :: Text)
            , pretty ("    \"credential\": \"script1...\"," :: Text)
            , pretty ("    \"role\": \"unknown\"," :: Text)
            , pretty ("    \"script_hash\": \"...\"" :: Text)
            , pretty ("}" :: Text)
            ])
  where
    parser = pure Inspect

run :: Cmd -> IO ()
run Inspect = do
    raw <- B8.filter noNewline <$> B8.hGetContents stdin
    either fail (BL8.hPutStrLn stdout . Json.encodePretty . scriptHashInputToJson)
        (parseScriptHashInput $ T.decodeUtf8 raw)

-- | A script hash together with the governance role it was encoded with, if any.
data ScriptHashInput = ScriptHashInput (Maybe KeyRole) ScriptHash

-- | Parse a script hash from text. Bech32 input is decoded using the
-- human-readable prefix to recover the governance role, if any. Base16 input
-- lacks this information, so it is interpreted by size: 28 bytes is a generic
-- script hash and 29 bytes (with the CIP-0129 header byte) is a governance
-- script credential.
parseScriptHashInput :: Text -> Either String ScriptHashInput
parseScriptHashInput txt =
    case detectEncoding (unpack txt) of
        Just EBase16 -> parseRaw (fromBase16 $ T.encodeUtf8 txt)
        Just (EBech32 ()) -> parseBech32
        _ -> Left unexpectedEncoding
  where
    parseRaw :: Either String ByteString -> Either String ScriptHashInput
    parseRaw = either Left checkRaw

    checkRaw :: ByteString -> Either String ScriptHashInput
    checkRaw bytes = case BS.length bytes of
        28 -> Right (ScriptHashInput Nothing (ScriptHash bytes))
        29 -> case BS.uncons bytes of
            Just (0x23, payload) ->
                Right (ScriptHashInput (Just Representative) (ScriptHash payload))
            Just (0x13, payload) ->
                Right (ScriptHashInput (Just CommitteeCold) (ScriptHash payload))
            Just (0x03, payload) ->
                Right (ScriptHashInput (Just CommitteeHot) (ScriptHash payload))
            _ -> Left errCredentialCIP0129
        n -> Left $ "Invalid script hash size: " <> show n
            <> " bytes. Expected 28 or 29 bytes."

    parseBech32 :: Either String ScriptHashInput
    parseBech32 = do
        (hrp, _) <- first (const unexpectedEncoding) $
            fromBech32 (\_ s -> s) (T.encodeUtf8 txt)
        h <- first prettyErrScriptHashFromText (scriptHashFromText txt)
        pure $ ScriptHashInput (roleForHrp hrp) h

    errCredentialCIP0129 =
        "Couldn't convert bytes into a CIP-0129 credential. Expected a DRep, \
        \committee cold or committee hot script credential."
    unexpectedEncoding =
        "Couldn't detect input encoding? Data on stdin must be encoded as \
        \bech32 or base16."

-- | Governance role implied by a bech32 human-readable prefix. A generic
-- script hash (\"script\") carries no governance role.
roleForHrp :: HumanReadablePart -> Maybe KeyRole
roleForHrp hrp
    | hrp == CIP5.drep || hrp == CIP5.drep_script =
        Just Representative
    | hrp == CIP5.cc_cold || hrp == CIP5.cc_cold_script =
        Just CommitteeCold
    | hrp == CIP5.cc_hot || hrp == CIP5.cc_hot_script =
        Just CommitteeHot
    | otherwise =
        Nothing

scriptHashInputToJson :: ScriptHashInput -> Json.Value
scriptHashInputToJson (ScriptHashInput mRole sh) = Json.object $
    [ "script_hash" .= Json.String (base16 $ unScriptHash sh)
    , "role" .= Json.String (maybe "unknown" keyRoleToText mRole)
    , "credential" .= Json.String (credentialText mRole sh)
    ]
    <> legacyField mRole sh
  where
    credentialText Nothing h = genericCredential h
    credentialText (Just r) h = scriptHashToText h r (Just CIP0129)

    legacyField (Just r) h =
        [ "legacy_credential" .= Json.String (scriptHashToText h r (Just CIP0105)) ]
    legacyField Nothing _ = []

    genericCredential h =
        T.decodeUtf8 $ encode (EBech32 CIP5.script) (unScriptHash h)

base16 :: ByteString -> Text
base16 = T.decodeUtf8 . encode EBase16
