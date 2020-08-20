{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Inspect
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( Address, unsafeMkAddress )
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
import Control.Exception
    ( SomeException, displayException )
import Control.Monad.Error.Class
    ( Error )
import Fmt
    ( format )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import System.Exit
    ( die )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBytes, progName )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BL8


data Cmd = Inspect
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "inspect" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Show information about an address"
        <> footerDoc (Just $ vsep
            [ string "The address is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string "$ cat addr.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address payment --network-tag 0 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address delegation $(cat stake.prv | "<>progName<>" key public) \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address inspect"
            , indent 2 $ string "{"
            , indent 2 $ string "    \"address_style\": \"Shelley\","
            , indent 2 $ string "    \"stake_reference\": \"by value\","
            , indent 2 $ string "    \"stake_key_hash\": \"6b542d6da35e6c95d95a33c6f66ec482d3f4caf3ad35e2ede09cf827\","
            , indent 2 $ string "    \"spending_key_hash\": \"44bc4f524f49a78a9c8a45b882d8710cb9254b3da6a978d50dc9b870\","
            , indent 2 $ string "    \"network_tag\": 0"
            , indent 2 $ string "}"
            ])
  where
    parser = pure Inspect


-- used for 'inspect'
instance Error SomeException

run :: Cmd -> IO ()
run Inspect = do
    bytes <- hGetBytes stdin
    case inspect (unsafeMkAddress bytes) of
      Right json -> BL8.hPutStrLn stdout (Json.encodePretty json)
      Left  e    -> die $ format "Error: {}" (displayException e)
  where
    -- We can't use IO here, because (<|>) in IO only catches IOException type,
    -- but MonadThrow in IO doesn't specialize to IOException.
    --
    -- Luckily, there's an existing instance for:
    --   instance (Error e) => Alternative (Either e)
    inspect :: (e ~ SomeException) => Address -> Either e Json.Value
    inspect addr = do
      foldr1 (<|>)
        [inspectByronAddress       addr
        ,inspectIcarusAddress      addr
        ,inspectJormungandrAddress addr
        ,inspectShelleyAddress     addr]
