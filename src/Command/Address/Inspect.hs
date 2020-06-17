{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Address.Inspect
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( unsafeMkAddress )
import Cardano.Address.Style.Byron
    ( inspectByronAddress )
import Cardano.Address.Style.Icarus
    ( inspectIcarusAddress )
import Cardano.Address.Style.Jormungandr
    ( inspectJormungandrAddress )
import Cardano.Address.Style.Shelley
    ( inspectShelleyAddress )
import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBytes )

import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BL8


data Cmd = Inspect
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "inspect" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Show information about an address"
        <> footerDoc (Just $ string $ mconcat
            [ "The address is read from stdin.\n"
            , "\n"
            , "Example:\n"
            , "  $ cat addr.prv \\\n"
            , "  | cardano-address key public \\\n"
            , "  | cardano-address address payment --network-tag 0 \\\n"
            , "  | cardano-address address delegation $(cat stake.prv | cardano-address key public) \\\n"
            , "  | cardano-address address inspect\n"
            , "  {\n"
            , "      \"stake_reference\": \"by value\",\n"
            , "      \"stake_key_hash\": \"6b542d6da35e6c95d95a33c6f66ec482d3f4caf3ad35e2ede09cf827\",\n"
            , "      \"address_style\": \"Shelley\",\n"
            , "      \"spending_key_hash\": \"44bc4f524f49a78a9c8a45b882d8710cb9254b3da6a978d50dc9b870\",\n"
            , "      \"network_tag\": 0\n"
            , "  }\n"
            ])
  where
    parser = pure Inspect

run :: Cmd -> IO ()
run Inspect = do
    bytes <- hGetBytes stdin
    case inspect (unsafeMkAddress bytes) of
        Nothing   -> fail "Unrecognized address on standard input"
        Just json -> BL8.hPutStrLn stdout (Json.encodePretty json)
  where
    inspect addr =
        inspectByronAddress       addr <|>
        inspectIcarusAddress      addr <|>
        inspectJormungandrAddress addr <|>
        inspectShelleyAddress     addr
