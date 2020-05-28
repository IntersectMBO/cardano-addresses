{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Address.Init
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Data.Binary.Put
    ( putWord32be )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Encoding
    ( AbstractEncoding (..) )
import Options.Applicative.Help.Pretty
    ( string )
import Options.Applicative.Style
    ( Style (..), encodeWithStyle, styleArg )
import System.IO
    ( stdout )
import System.IO.Extra
    ( hPutBytes )

data Cmd = Cmd
    { networkTag :: NetworkTag
    , style :: Style
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "init" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Initialize a blank address."
        <> footerDoc (Just $ string $ mconcat
            [ "The result is invalid as-such and needs to be chained through "
            , "other commands in order to add components to the address."
            ])
  where
    parser = Cmd
        <$> networkTagOpt
        <*> styleArg

run :: Cmd -> IO ()
run Cmd{networkTag,style} =
    hPutBytes stdout (encodeWithStyle style putNetworkTag) EBase16
  where
    putNetworkTag = putWord32be . unNetworkTag $ networkTag
