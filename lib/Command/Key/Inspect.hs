{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Key.Inspect
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( XPrv
    , XPub
    , xprvChainCode
    , xprvFromBytes
    , xprvPrivateKey
    , xpubChainCode
    , xpubFromBytes
    , xpubPublicKey
    )
import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartToText )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , encode
    , fromBase16
    , fromBase58
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
        <> progDesc "Show information about a key"
        <> footerDoc (Just $ vsep
            [ pretty ("The key (public or private) is read from stdin." :: Text)
            , pretty ("It can be bech32-, base16- or base58-encoded." :: Text)
            , pretty ("" :: Text)
            , pretty ("Example:" :: Text)
            , pretty ("$ cardano-address key inspect <<< $(cat acct.prv)" :: Text)
            , pretty ("{" :: Text)
            , pretty ("    \"key_type\": \"private\"," :: Text)
            , pretty ("    \"chain_code\": \"...\"," :: Text)
            , pretty ("    \"extended_key\": \"...\"" :: Text)
            , pretty ("}" :: Text)
            ])
  where
    parser = pure Inspect

run :: Cmd -> IO ()
run Inspect = do
    raw <- B8.filter noNewline <$> B8.hGetContents stdin
    either fail (BL8.hPutStrLn stdout . Json.encodePretty . keyInputToJson)
        (parseKeyInput $ T.decodeUtf8 raw)

-- | The result of parsing a key read from stdin.
data KeyInput
    = XPubInput XPub
    | XPrvInput XPrv
    | RawPubInput ByteString
    | RawPrvInput ByteString

-- | Parse a key from text. Bech32 input is decoded using the human-readable
-- prefix to tell apart public from private keys and extended keys from
-- non-extended ones. Base16 and base58 input lacks this information, so the
-- key kind is inferred from the payload size:
--
-- - 96 bytes: extended private key
-- - 64 bytes: extended public key
-- - 32 bytes: non-extended public key
--
-- Because a non-extended private key has the same size as an extended public
-- key, it cannot be told apart from raw bytes and must be given in bech32
-- (as @*_sk@) to be inspected.
parseKeyInput :: Text -> Either String KeyInput
parseKeyInput txt =
    case detectEncoding (unpack txt) of
        Just EBase16 -> decodeRaw (fromBase16 $ T.encodeUtf8 txt)
        Just EBase58 -> decodeRaw (fromBase58 $ T.encodeUtf8 txt)
        Just (EBech32 ()) -> decodeBech32
        Nothing -> Left unexpectedEncoding
  where
    decodeRaw = either Left parseRawBytes

    decodeBech32 = do
        (hrp, bytes) <- first (const unexpectedEncoding) $
            fromBech32 (\_ s -> s) (T.encodeUtf8 txt)
        parseBech32 hrp bytes

    parseRawBytes :: ByteString -> Either String KeyInput
    parseRawBytes bytes = case BS.length bytes of
        96 -> maybe (Left errXPrv) (Right . XPrvInput) (xprvFromBytes bytes)
        64 -> maybe (Left errXPub) (Right . XPubInput) (xpubFromBytes bytes)
        32 -> Right (RawPubInput bytes)
        n  -> Left $ "Invalid key size: " <> show n
            <> " bytes. Expected 32, 64 or 96 bytes."

    parseBech32 :: HumanReadablePart -> ByteString -> Either String KeyInput
    parseBech32 hrp bytes
        | hrp `elem` extendedPubHrps =
            maybe (Left errXPub) (Right . XPubInput) (xpubFromBytes bytes)
        | hrp `elem` extendedPrvHrps =
            maybe (Left errXPrv) (Right . XPrvInput) (xprvFromBytes bytes)
        | hrp `elem` nonExtendedPubHrps =
            if BS.length bytes == 32
                then Right (RawPubInput bytes)
                else Left errXPub
        | hrp `elem` nonExtendedPrvHrps =
            if BS.length bytes == 64
                then Right (RawPrvInput bytes)
                else Left errXPrv
        | otherwise = Left invalidHrp

    errXPub =
        "Couldn't convert bytes into extended public key."
    errXPrv =
        "Couldn't convert bytes into extended private key."
    invalidHrp =
        "Invalid human-readable prefix. Prefix ought to be one of: "
        <> show (showHrp <$> allKeyHrps)
    unexpectedEncoding =
        "Couldn't detect input encoding? Data on stdin must be encoded as \
        \bech32, base16 or base58."

showHrp :: HumanReadablePart -> String
showHrp = unpack . humanReadablePartToText

extendedPubHrps :: [HumanReadablePart]
extendedPubHrps =
    [ CIP5.root_xvk
    , CIP5.acct_xvk
    , CIP5.addr_xvk
    , CIP5.stake_xvk
    , CIP5.policy_xvk
    , CIP5.root_shared_xvk
    , CIP5.acct_shared_xvk
    , CIP5.addr_shared_xvk
    , CIP5.stake_shared_xvk
    , CIP5.drep_xvk
    , CIP5.cc_cold_xvk
    , CIP5.cc_hot_xvk
    ]

extendedPrvHrps :: [HumanReadablePart]
extendedPrvHrps =
    [ CIP5.root_xsk
    , CIP5.acct_xsk
    , CIP5.addr_xsk
    , CIP5.stake_xsk
    , CIP5.policy_xsk
    , CIP5.root_shared_xsk
    , CIP5.acct_shared_xsk
    , CIP5.addr_shared_xsk
    , CIP5.stake_shared_xsk
    , CIP5.drep_xsk
    , CIP5.cc_cold_xsk
    , CIP5.cc_hot_xsk
    ]

nonExtendedPubHrps :: [HumanReadablePart]
nonExtendedPubHrps =
    [ CIP5.root_vk
    , CIP5.acct_vk
    , CIP5.addr_vk
    , CIP5.stake_vk
    , CIP5.policy_vk
    , CIP5.root_shared_vk
    , CIP5.acct_shared_vk
    , CIP5.addr_shared_vk
    , CIP5.stake_shared_vk
    , CIP5.drep_vk
    , CIP5.cc_cold_vk
    , CIP5.cc_hot_vk
    ]

nonExtendedPrvHrps :: [HumanReadablePart]
nonExtendedPrvHrps =
    [ CIP5.root_sk
    , CIP5.acct_sk
    , CIP5.addr_sk
    , CIP5.stake_sk
    , CIP5.policy_sk
    , CIP5.root_shared_sk
    , CIP5.acct_shared_sk
    , CIP5.addr_shared_sk
    , CIP5.stake_shared_sk
    , CIP5.drep_sk
    , CIP5.cc_cold_sk
    , CIP5.cc_hot_sk
    ]

allKeyHrps :: [HumanReadablePart]
allKeyHrps =
    extendedPubHrps <> extendedPrvHrps <> nonExtendedPubHrps <> nonExtendedPrvHrps

base16 :: ByteString -> Text
base16 = T.decodeUtf8 . encode EBase16

keyInputToJson :: KeyInput -> Json.Value
keyInputToJson = \case
    XPubInput xpub -> Json.object
        [ "key_type" .= Json.String "public"
        , "extended_key" .= Json.String (base16 $ xpubPublicKey xpub)
        , "chain_code" .= Json.String (base16 $ xpubChainCode xpub)
        ]
    XPrvInput xprv -> Json.object
        [ "key_type" .= Json.String "private"
        , "extended_key" .= Json.String (base16 $ xprvPrivateKey xprv)
        , "chain_code" .= Json.String (base16 $ xprvChainCode xprv)
        ]
    RawPubInput bytes -> Json.object
        [ "key_type" .= Json.String "public"
        , "key" .= Json.String (base16 bytes)
        ]
    RawPrvInput bytes -> Json.object
        [ "key_type" .= Json.String "private"
        , "key" .= Json.String (base16 bytes)
        ]
