{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.FromRecoveryPhrase
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( xprvToBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Encoding
    ( Encoding, encodingOpt )
import Options.Applicative.Help.Pretty
    ( string )
import Options.Applicative.Style
    ( Style, generateRootKey, styleArg )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetSomeMnemonic, hPutBytes )


data Cmd = FromRecoveryPhrase
    { encoding :: Encoding
    , style :: Style
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "from-recovery-phrase" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Convert a recovery phrase to an extended private key"
        <> footerDoc (Just $ string $ mconcat
            [ "The recovery phrase is read from stdin."
            , "\n\n"
            , "Example:\n\n"
            , "  $ cardano-address recovery-phrase generate \\\n"
            , "  | cardano-address key from-recovery-phrase icarus \\\n"
            ])
  where
    parser = FromRecoveryPhrase
        <$> encodingOpt [humanReadablePart|xprv|]
        <*> styleArg

run :: Cmd -> IO ()
run FromRecoveryPhrase{encoding,style} = do
    someMnemonic <- hGetSomeMnemonic stdin
    rootK <- generateRootKey someMnemonic style
    hPutBytes stdout (xprvToBytes rootK) encoding
