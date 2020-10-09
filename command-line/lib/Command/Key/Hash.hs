{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Hash
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Style.Shelley
    ( hashKey, liftXPub )
import Cardano.Script
    ( KeyHash (..) )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Encoding
    ( Encoding, encodingOpt )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPub, hPutBytes )

newtype Cmd = Hash
    { encoding :: Encoding
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the hash of a public key"
        <> footerDoc (Just $ string $ mconcat
            [ "The public key is read from stdin."
            ])
  where
    parser = Hash
        <$> encodingOpt [humanReadablePart|xpub_hash|]

run :: Cmd -> IO ()
run Hash{encoding} = do
    xpub <- hGetXPub stdin
    let (KeyHash hash) = hashKey (liftXPub xpub)
    hPutBytes stdout hash encoding
