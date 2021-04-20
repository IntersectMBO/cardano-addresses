{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Style
    ( Style (..), generateRootKey, styleArg )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetSomeMnemonic, hPutBytes, progName )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5


newtype Cmd = FromRecoveryPhrase
    { style :: Style
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "from-recovery-phrase" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Convert a recovery phrase to an extended private key"
        <> footerDoc (Just $ vsep
            [ string "The recovery phrase is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate \\"
            , indent 2 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Icarus"
            ])
  where
    parser = FromRecoveryPhrase
        <$> styleArg

run :: Cmd -> IO ()
run FromRecoveryPhrase{style} = do
    someMnemonic <- hGetSomeMnemonic stdin
    rootK <- generateRootKey someMnemonic style
    case style of
        Shared ->
            hPutBytes stdout (xprvToBytes rootK) (EBech32 CIP5.shared_root_xsk)
        _ ->
            hPutBytes stdout (xprvToBytes rootK) (EBech32 CIP5.root_xsk)
