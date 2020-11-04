{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Public
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( toXPub, xpubPublicKey, xpubToBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Encoding
    ( Encoding, encodingOpt )
import Options.Applicative.Help.Pretty
    ( string )
import Options.Applicative.Public
    ( PublicType (..), publicOpt )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPrv, hPutBytes )


data Cmd = Public
    { encoding :: Encoding
    , chainCode :: PublicType
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "public" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the public counterpart of a private key"
        <> footerDoc (Just $ string $ mconcat
            [ "The private key is read from stdin."
            , "To get extended public key pass '--with-chain-code'."
            , "To get public key pass '--without-chain-code'."
            ])
  where
    parser = Public
        <$> encodingOpt [humanReadablePart|xpub|]
        <*> publicOpt

run :: Cmd -> IO ()
run Public{encoding, chainCode} = do
    xprv <- hGetXPrv stdin
    let xpub = toXPub xprv
    bytes <- case chainCode of
        WithChainCode -> pure $ xpubToBytes xpub
        WithoutChainCode -> pure $ xpubPublicKey xpub
    hPutBytes stdout bytes encoding
