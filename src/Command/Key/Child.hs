{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Child
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( deriveXPrv, deriveXPub, xprvToBytes, xpubToBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Monad
    ( foldM )
import Data.Functor.Identity
    ( Identity (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Derivation
    ( DerivationPath
    , DerivationScheme
    , castDerivationPath
    , derivationPathArg
    , derivationSchemeOpt
    )
import Options.Applicative.Encoding
    ( Encoding, encodingOpt )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( failWith, hGetXP__, hPutBytes )


data Cmd = Child
    { encoding :: Encoding
    , scheme :: DerivationScheme
    , path :: DerivationPath
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "child" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Derive child keys from a parent public/private key."
        <> footerDoc (Just $ string $ mconcat
            [ "The parent key is read from stdin."
            ])
  where
    parser = Child
        <$> encodingOpt [humanReadablePart|???|]
        <*> derivationSchemeOpt
        <*> derivationPathArg

run :: Cmd -> IO ()
run Child{encoding,path,scheme} = do
    xkey <- hGetXP__ stdin

    (child,encoding') <- case xkey of
        Left xpub -> do
            let ixs = castDerivationPath path
            case foldM (deriveXPub scheme) xpub ixs of
                Nothing ->
                    failWith
                        "Couldn't derive child key. If you're trying to derive \
                        \children on a PUBLIC key, you must use soft indexes only."
                Just child ->
                    pure
                        ( xpubToBytes child
                        , fmap (const [humanReadablePart|xpub|] ) encoding
                        )

        Right xprv -> do
            let ixs = castDerivationPath path
            let Identity child = foldM (\k -> pure . deriveXPrv scheme k) xprv ixs
            pure
                ( xprvToBytes child
                , fmap (const [humanReadablePart|xprv|]) encoding
                )

    hPutBytes stdout child encoding'
