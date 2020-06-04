{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Inspect
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvChainCode, xprvPrivateKey, xpubChainCode, xpubPublicKey )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Encoding
    ( AbstractEncoding (..) )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXP__, hPutBold, hPutBytes )

import qualified Data.ByteString.Char8 as B8


data Cmd = Inspect
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "inspect" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Show information about a key"
        <> footerDoc (Just $ string $ mconcat
            [ "The parent key is read from stdin."
            ])
  where
    parser = pure Inspect

run :: Cmd -> IO ()
run Inspect = do
    either inspectXPub inspectXPrv =<< hGetXP__ stdin
  where
    newline = B8.hPutStrLn stdout ""

    inspectXPub :: XPub -> IO ()
    inspectXPub xpub = do
        hPutBold stdout "key type:     " *> B8.hPutStr stdout "public"    *> newline
        hPutBold stdout "extended key: " *> hPutBytes  stdout pub EBase16 *> newline
        hPutBold stdout "chain code:   " *> hPutBytes  stdout cc  EBase16 *> newline
      where
        pub = xpubPublicKey xpub
        cc  = xpubChainCode xpub

    inspectXPrv :: XPrv -> IO ()
    inspectXPrv xprv = do
        hPutBold stdout "key type:     " *> B8.hPutStr stdout "private"   *> newline
        hPutBold stdout "extended key: " *> hPutBytes  stdout prv EBase16 *> newline
        hPutBold stdout "chain code:   " *> hPutBytes  stdout cc  EBase16 *> newline
      where
        prv = xprvPrivateKey xprv
        cc  = xprvChainCode  xprv
