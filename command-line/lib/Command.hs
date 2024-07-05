{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Command
    ( CLI

    -- * I/O interface
    , setup
    , parse
    , run

    -- * I/O Fix
    , withUtf8Encoding
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
import Options.Applicative.Help.Pretty.Compat
    ( bold, hsep, string, vsep )
import System.Console.ANSI
    ( hNowSupportsANSI )
import System.IO
    ( BufferMode (..)
    , Handle
    , hSetBuffering
    , hSetEncoding
    , mkTextEncoding
    , stderr
    , stdin
    , stdout
    )
import System.IO.CodePage
    ( withCP65001 )
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
        [ string "💡 Need auto-completion?"
        , string ""
        , hsep
            [ string "  ↳"
            , bold $ string "source <("
            , bold $ string progName
            , bold $ string $ "--bash-completion-script `which "<>progName<>"`)"
            ]
        , string ""
        , string "Or alternatively --fish-completion-script / --zsh-completion-script."
        , string "For a long-term solution, you may want to put this script in the relevant place. e.g.:"
        , string ""
        , hsep [string "  ↳", bold $ string "/etc/bash_completion.d"]
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
      void $ hNowSupportsANSI h
      hSetBuffering h NoBuffering

-- | Force the locale text encoding to UTF-8. This is needed because the CLI
-- prints UTF-8 characters regardless of the @LANG@ environment variable or any
-- other settings.
--
-- On Windows the current console code page is changed to UTF-8.
withUtf8Encoding :: IO a -> IO a
withUtf8Encoding action = withCP65001 (setUtf8EncodingHandles >> action)
  where
    setUtf8EncodingHandles :: IO ()
    setUtf8EncodingHandles = do
        utf8' <- mkTextEncoding "UTF-8//TRANSLIT"
        mapM_ (`hSetEncoding` utf8') [stdin, stdout, stderr]
