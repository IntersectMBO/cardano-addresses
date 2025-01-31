{-# LANGUAGE FlexibleContexts #-}
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
    ( XPrv, XPub, xprvChainCode, xprvPrivateKey, xpubChainCode, xpubPublicKey )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Data.Aeson
    ( (.=) )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( pretty )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXP__ )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding as T


data Cmd = Inspect
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "inspect" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Show information about a key"
        <> footerDoc (Just $ pretty $ mconcat
            [ "The parent key is read from stdin." :: Text
            ])
  where
    parser = pure Inspect

run :: Cmd -> IO ()
run Inspect = do
    either inspectXPub inspectXPrv =<< hGetXP__ stdin allowedPrefixes
  where
    allowedPrefixes :: [HumanReadablePart]
    allowedPrefixes =
        [ CIP5.root_xvk
        , CIP5.root_xsk
        , CIP5.acct_xvk
        , CIP5.acct_xsk
        , CIP5.addr_xvk
        , CIP5.addr_xsk
        , CIP5.stake_xvk
        , CIP5.stake_xsk
        , CIP5.policy_xvk
        , CIP5.policy_xsk
        , CIP5.root_shared_xvk
        , CIP5.root_shared_xsk
        , CIP5.acct_shared_xvk
        , CIP5.acct_shared_xsk
        , CIP5.addr_shared_xvk
        , CIP5.addr_shared_xsk
        , CIP5.stake_shared_xvk
        , CIP5.stake_shared_xsk
        , CIP5.drep_xvk
        , CIP5.drep_xsk
        , CIP5.cc_cold_xvk
        , CIP5.cc_cold_xsk
        , CIP5.cc_hot_xvk
        , CIP5.cc_hot_xsk
        ]

    base16 :: ByteString -> Text
    base16 = T.decodeUtf8 . encode EBase16

    inspectXPub :: (HumanReadablePart, XPub) -> IO ()
    inspectXPub (_, xpub) = do
        BL8.hPutStrLn stdout $ Json.encodePretty $ Json.object
            [ "key_type" .= Json.String "public"
            , "extended_key" .= Json.String (base16 pub)
            , "chain_code" .= Json.String (base16 cc)
            ]
      where
        pub = xpubPublicKey xpub
        cc  = xpubChainCode xpub

    inspectXPrv :: (HumanReadablePart, XPrv) -> IO ()
    inspectXPrv (_, xprv) =
        BL8.hPutStrLn stdout $ Json.encodePretty $ Json.object
            [ "key_type" .= Json.String "private"
            , "extended_key" .= Json.String (base16 prv)
            , "chain_code" .= Json.String (base16 cc)
            ]
      where
        prv = xprvPrivateKey xprv
        cc  = xprvChainCode  xprv
