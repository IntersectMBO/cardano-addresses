{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Address.Inspect
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( Address, unsafeMkAddress )
import Cardano.Address.Derivation
    ( XPub )
import Control.Applicative
    ( optional )
import Control.Exception
    ( displayException )
import Data.Text
    ( Text )
import Fmt
    ( format )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Derivation
    ( xpubOpt )
import Options.Applicative.Help.Pretty
    ( Doc, annotate, bold, indent, pretty, vsep )
import System.Exit
    ( die )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBytes, progName )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BL8

newtype Cmd = Inspect
    { rootPublicKey :: Maybe XPub
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "inspect" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Show information about an address"
        <> footerDoc (Just $ vsep
            [ prettyText "The address is read from stdin."
            , prettyText ""
            , prettyText "Example:"
            , indent 2 $ annotate bold $ prettyText "$ cat addr.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address payment --network-tag testnet \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address delegation $(cat stake.prv | "<>progName<>" key public --with-chain-code) \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address inspect"
            , indent 2 $ prettyText "{"
            , indent 2 $ prettyText "    \"address_style\": \"Shelley\","
            , indent 2 $ prettyText "    \"stake_reference\": \"by value\","
            , indent 2 $ prettyText "    \"stake_key_hash\": \"6b542d6da35e6c95d95a33c6f66ec482d3f4caf3ad35e2ede09cf827\","
            , indent 2 $ prettyText "    \"spending_key_hash\": \"44bc4f524f49a78a9c8a45b882d8710cb9254b3da6a978d50dc9b870\","
            , indent 2 $ prettyText "    \"network_tag\": 0,"
            , indent 2 $ prettyText "    \"address_type\": 0"
            , indent 2 $ prettyText "}"
            ])
  where
    parser = Inspect
        <$> optional (xpubOpt [CIP5.root_xvk] "root" helpDoc)
    helpDoc =
        "A root public key. If specified, tries to decrypt the derivation path \
        \of Byron addresses."

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Inspect{rootPublicKey} = do
    bytes <- hGetBytes stdin
    case inspect (unsafeMkAddress bytes) of
      Right json -> BL8.hPutStrLn stdout (Json.encodePretty json)
      Left  e    -> die $ format "Error: {}" (displayException e)
  where
    inspect :: Address -> Either Shelley.ErrInspectAddress Shelley.InspectAddress
    inspect = Shelley.eitherInspectAddress rootPublicKey
