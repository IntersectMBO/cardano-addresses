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
import Control.Monad
    ( forM_ )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBytes, hPutBold )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data Cmd = Inspect
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "inspect" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Show information about an address."
        <> footerDoc (Just $ string $ mconcat
            [ "The address is read from stdin.\n"
            , "\n"
            , "Example:\n"
            , "  $ cat addr.prv \\\n"
            , "  | cardano-address key public \\\n"
            , "  | cardano-address address payment --network-tag 0 \\\n"
            , "  | cardano-address address delegation $(cat stake.prv | cardano-address key public) \\\n"

            , "  | cardano-address address inspect\n"
            , "\n"
            , "  address style:      Shelley\n"
            , "  address type:       base\n"
            , "  spending key hash:  79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65\n"
            , "  stake key hash:     cc339a35f9e0fe039cf510c761d4dd29040c48e9657fdac7e9c01d94\n"
            , "  network tag:        0"
            ])
  where
    parser = pure Inspect

run :: Cmd -> IO ()
run Inspect = do
    bytes <- hGetBytes stdin
    case inspect (unsafeMkAddress bytes) of
        Nothing -> fail "Unrecognized address on standard input."
        Just str -> forM_ (T.pack <$> lines str) $ \line -> do
            let (label, value) = T.breakOn ":" line
            hPutBold     stdout (T.encodeUtf8 label)
            B8.hPutStrLn stdout (T.encodeUtf8 value)
  where
    inspect addr =
        inspectByronAddress       addr <|>
        inspectIcarusAddress      addr <|>
        inspectJormungandrAddress addr <|>
        inspectShelleyAddress     addr
