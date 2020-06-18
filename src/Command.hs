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
    , footerDoc
    , helper
    , info
    , prefs
    , progDesc
    , showHelpOnEmpty
    , subparser
    )
import Options.Applicative.Help.Pretty
    ( bold, hsep, string, vsep )
import System.Console.ANSI
    ( hSupportsANSIWithoutEmulation )
import System.IO
    ( BufferMode (..), Handle, hSetBuffering, stderr, stdout )
import System.IO.Extra
    ( prettyIOException )

import qualified Command.Address as Address
import qualified Command.Key as Key
import qualified Command.RecoveryPhrase as RecoveryPhrase


data CLI
    = RecoveryPhrase RecoveryPhrase.Cmd
    | Key Key.Cmd
    | Address Address.Cmd
    deriving (Show)

cli :: ParserInfo CLI
cli = info (helper <*> parser) $ mempty
    <> progDesc "cardano-addresses"
    <> footerDoc (Just $ vsep
        [ string "💡 Need auto-completion?"
        , string ""
        , hsep [string "  ↳", bold $ string "source <(cardano-address --bash-completion-script `which cardano-address`)"]
        , string ""
        , string "Or alternatively --fish-completion-script / --zsh-completion-script."
        , string "For a long-term solution, you may want to put this script in the relevant place. e.g.:"
        , string ""
        , hsep [string "  ↳", bold $ string "/etc/bash_completion.d"]
        ])
  where
    parser = subparser $ mconcat
        [ RecoveryPhrase.mod RecoveryPhrase
        , Key.mod Key
        , Address.mod Address
        ]

-- | Run a given command
run :: CLI -> IO ()
run = handle prettyIOException . \case
    RecoveryPhrase sub -> RecoveryPhrase.run sub
    Key sub -> Key.run sub
    Address sub -> Address.run sub

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
