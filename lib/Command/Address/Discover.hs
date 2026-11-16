{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Address.Discover
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( bech32 )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index
    , XPrv
    , XPub
    , indexFromWord32
    , toXPub
    )
import Cardano.Address.Discovery
    ( DiscoveredAddress (..)
    , DiscoveredCredential (..)
    , DiscoveryAddressType (..)
    , DiscoveryResult (..)
    , discoverFrom
    , prettyErrDiscovery
    )
import Cardano.Address.KeyHash
    ( GovernanceType (..), keyHashToText )
import Cardano.Address.Style.Shelley
    ( Shelley, liftXPrv, liftXPub )
import Codec.Binary.Bech32
    ( humanReadablePartToText )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Discovery
    ( discoveryAddressTypeOpt
    , discoveryFromOpt
    , discoveryGapOpt
    , discoveryJsonOpt
    )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Help.Pretty
    ( Doc, annotate, bold, indent, pretty, vsep )
import Options.Applicative.Style
    ( Style (..) )
import System.Exit
    ( die )
import System.IO
    ( Handle, stdin, stdout )
import System.IO.Extra
    ( hGetXP__, progName )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

data Cmd = Cmd
    { gap :: !Word32
    , fromIndex :: !Word32
    , addressType :: !DiscoveryAddressType
    , networkTag :: !NetworkTag
    , outputJson :: !Bool
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "discover" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Discover addresses and credentials for a Shelley account"
        <> header "Derive a contiguous batch of payment addresses and account credentials."
        <> footerDoc (Just $ vsep
            [ prettyText "The account key is read from stdin. Either an account public key"
            , prettyText "(acct_xvk) / account private key (acct_xsk) or a root private key"
            , prettyText "(root_xsk) is accepted. A root key is used to derive the default"
            , prettyText "account (index 0)."
            , prettyText ""
            , prettyText "By default the result is emitted as tab-separated values (TSV)."
            , prettyText "Pass --json to emit a JSON document instead."
            , prettyText ""
            , prettyText "Example:"
            , indent 2 $ annotate bold $ pretty $ "$ cat acct.xvk | "<>progName<>" address discover \\"
            , indent 4 $ annotate bold $ prettyText "--gap 10 --type base --network-tag testnet"
            , indent 2 $ annotate bold $ prettyText "$ cat acct.xvk | cardano-address address discover \\"
            , indent 4 $ annotate bold $ prettyText "--gap 10 --type base --network-tag testnet --json"
            ])
  where
    parser = Cmd
        <$> discoveryGapOpt
        <*> discoveryFromOpt
        <*> discoveryAddressTypeOpt
        <*> networkTagOpt Shelley
        <*> discoveryJsonOpt

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Cmd{gap,fromIndex,addressType,networkTag,outputJson} = do
    accXPub <- readAccountKey stdin
    case discoverFrom networkTag addressType fromIndex gap accXPub of
        Left err -> die $ prettyErrDiscovery err
        Right result ->
            if outputJson then
                BL8.hPutStrLn stdout (Json.encodePretty result)
            else
                BL8.hPutStrLn stdout (renderTsv result)

-- | Read an account or root key from stdin and return the account public key.
readAccountKey :: Handle -> IO (Shelley 'AccountK XPub)
readAccountKey h = do
    hGetXP__ h allowedPrefixes >>= \case
        Left (hrp, xpub)
            | hrp == CIP5.acct_xvk ->
                pure (liftXPub xpub)
            | otherwise ->
                unsupported hrp
        Right (hrp, xprv)
            | hrp == CIP5.acct_xsk ->
                pure (toXPub <$> liftXPrv xprv)
            | hrp == CIP5.root_xsk ->
                deriveAccount0 (liftXPrv xprv)
            | otherwise ->
                unsupported hrp
  where
    allowedPrefixes =
        [ CIP5.acct_xvk
        , CIP5.acct_xsk
        , CIP5.root_xsk
        ]

    unsupported hrp = fail $ T.unpack $ T.unwords
        [ "Unsupported key prefix:"
        , humanReadablePartToText hrp <> "."
        , "Expected one of: acct_xvk, acct_xsk, root_xsk."
        ]

    deriveAccount0 :: Shelley 'RootK XPrv -> IO (Shelley 'AccountK XPub)
    deriveAccount0 rootXPrv =
        -- Account index 0 is hardened, so its raw Word32 representation is
        -- 0x80000000.
        case indexFromWord32 @(Index 'Hardened _) 0x80000000 of
            Nothing -> fail "Unable to construct account index 0."
            Just accIx -> pure $ toXPub <$> deriveAccountPrivateKey rootXPrv accIx

-- | Render a discovery result as tab-separated values.
renderTsv :: DiscoveryResult -> BL8.ByteString
renderTsv result = BL8.unlines $ concat
    [ map (row "payment_external") (paymentExternal result)
    , map (row "payment_internal") (paymentInternal result)
    , maybe [] (credentialRow "stake_credential") (stakeCredential result)
    , maybe [] (credentialRow "drep_credential") (drepCredential result)
    , maybe [] (credentialRow "committee_cold_credential") (committeeColdCredential result)
    , maybe [] (credentialRow "committee_hot_credential") (committeeHotCredential result)
    ]
  where
    row :: Text -> DiscoveredAddress -> BL8.ByteString
    row kind addr = BL8.pack $ T.unpack $ T.intercalate "\t"
        [ kind
        , T.pack (show (addressIndex addr))
        , bech32 (address addr)
        ]

    credentialRow :: Text -> DiscoveredCredential -> [BL8.ByteString]
    credentialRow kind cred =
        [ BL8.pack $ T.unpack $ T.intercalate "\t"
            [ kind
            , T.pack (show (credentialIndex cred))
            , keyHashToText (credential cred) CIP0129
            ]
        ]
