{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Command
    ( CLI

    -- * I/O interface
    , setup
    , parse
    , run
    ) where

import Prelude

import Control.Exception
    ( handle )
import Control.Monad
    ( void )
import Options.Applicative
    ( ParserInfo
    , customExecParser
    , helper
    , info
    , prefs
    , progDesc
    , progDesc
    , showHelpOnEmpty
    , subparser
    )
import System.Console.ANSI
    ( hSupportsANSIWithoutEmulation )
import System.IO
    ( BufferMode (..), Handle, hSetBuffering, stderr, stdout )
import System.IO.Extra
    ( prettyIOException )

import qualified Command.Key as Key
import qualified Command.RecoveryPhrase as RecoveryPhrase


data CLI
    = RecoveryPhrase RecoveryPhrase.Cmd
    | Key Key.Cmd
    deriving (Show)

cli :: ParserInfo CLI
cli = info (helper <*> parser) $ progDesc "cardano-addresses"
  where
    parser = subparser $ mconcat
        [ RecoveryPhrase.mod RecoveryPhrase
        , Key.mod Key
        ]

-- | Run a given command
run :: CLI -> IO ()
run = handle prettyIOException . \case
    RecoveryPhrase sub -> RecoveryPhrase.run sub
    Key sub -> Key.run sub

-- | Parse command line options and arguments
parse :: IO CLI
parse = customExecParser (prefs showHelpOnEmpty) cli

-- | Enable ANSI colors on Windows and correct output buffering
setup :: IO ()
setup =
    mapM_ hSetup [stderr, stdout]
  where
    hSetup :: Handle -> IO ()
    hSetup h = do
      void $ hSupportsANSIWithoutEmulation h
      hSetBuffering h NoBuffering
