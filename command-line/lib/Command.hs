{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

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
    , (<|>)
    )
import Options.Applicative.Help.Pretty
    ( annotate, bold, hsep, pretty, vsep )
import System.Console.ANSI
    ( hSupportsANSI )
import System.IO
    ( BufferMode (..), Handle, hSetBuffering, stderr, stdin, stdout )
import System.IO.Extra
    ( prettyIOException, progName )

import qualified Command.Address as Address
import qualified Command.Key as Key
import qualified Command.RecoveryPhrase as RecoveryPhrase
import qualified Command.Script as Script
import qualified Command.Version as Version

data CLI
    = RecoveryPhrase RecoveryPhrase.Cmd
    | Key Key.Cmd
    | Address Address.Cmd
    | Script Script.Cmd
    | Version
    deriving (Show)

cli :: ParserInfo CLI
cli = info (helper <*> parser) $ mempty
    <> progDesc "Command-line for address and key manipulation in Cardano."
    <> footerDoc (Just $ vsep
        [ pretty "💡 Need auto-completion?"
        , pretty ""
        , hsep
            [ pretty "  ↳"
            , annotate bold $ pretty "source <("
            , annotate bold $ pretty progName
            , annotate bold $ pretty $ "--bash-completion-script `which "<>progName<>"`)"
            ]
        , pretty ""
        , pretty "Or alternatively --fish-completion-script / --zsh-completion-script."
        , pretty "For a long-term solution, you may want to put this script in the relevant place. e.g.:"
        , pretty ""
        , hsep [pretty "  ↳", annotate bold $ pretty "/etc/bash_completion.d"]
        ])
  where
    parser = Version.opt Version <|> subparser (mconcat
        [ RecoveryPhrase.mod RecoveryPhrase
        , Key.mod Key
        , Address.mod Address
        , Script.mod Script
        ])

-- | Run a given command
run :: CLI -> IO ()
run = handle prettyIOException . \case
    RecoveryPhrase sub -> RecoveryPhrase.run sub
    Key sub -> Key.run sub
    Address sub -> Address.run sub
    Script sub -> Script.run sub
    Version -> Version.run

-- | Parse command line options and arguments
parse :: IO CLI
parse = customExecParser (prefs showHelpOnEmpty) cli

-- | Enable ANSI colors on Windows and correct output buffering
setup :: IO ()
setup =
    mapM_ hSetup [stderr, stdout, stdin]
  where
    hSetup :: Handle -> IO ()
    hSetup h = do
      void $ hSupportsANSI h
      hSetBuffering h NoBuffering
