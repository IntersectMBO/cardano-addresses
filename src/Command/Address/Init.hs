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

import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Monad
    ( foldM )
import Data.Functor.Identity
    ( Identity (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Encoding
    ( Encoding, encodingOpt )
import Options.Applicative.Help.Pretty
    ( string )
import Options.Applicative.Style
    ( Style, styleArg )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXP__, hPutBytes )


data Cmd = Cmd
    { encoding :: Encoding
    , networkTag :: NetworkTag
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
        <$> encodingOpt [humanReadablePart|addr|]
        <*> networkTagOpt
        <*> styleArg

run :: Cmd -> IO ()
run Cmd{encoding,networkTag,style} = do
    fail "TODO"
