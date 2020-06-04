{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Delegation
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( bech32, unsafeMkAddress )
import Cardano.Address.Derivation
    ( XPub )
import Cardano.Address.Style.Shelley
    ( ErrExtendAddress (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Derivation
    ( xpubArg )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBytes )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T


newtype Cmd = Cmd
    { xpub :: XPub
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "delegation" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a delegation address"
        <> header "A payment address must be provided on stdin."
        <> footerDoc (Just $ string $ mconcat
            [ "Example:\n\n"
            , "    $ cardano-address recovery-phrase generate --size 15 \\\n"
            , "      | cardano-address key from-recovery-phrase Shelley > root.prv\n"
            , "\n"
            , "    $ cat root.prv \\\n"
            , "      | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv\n"
            , "\n"
            , "    $ cat root.prv \\\n"
            , "      | cardano-address key child 1852H/1815H/0H/0/0 > addr0.prv\n"
            , "\n"
            , "    $ cat addr0.prv \\\n"
            , "      | cardano-address key public \\\n"
            , "      | cardano-address address payment --network-tag 0 \\\n"
            , "      | cardano-address address delegation $(cat stake.prv | cardano-address key public)\n"
            , "\n"
            , "    addr1qpj2d4dqzds5p3mmlu95v9pex2d72cdvyjh2u3dtj4yqesv27k..."
            ])
  where
    parser = Cmd
        <$> xpubArg

run :: Cmd -> IO ()
run Cmd{xpub} = do
    bytes <- hGetBytes stdin
    case Shelley.extendAddress (unsafeMkAddress bytes) (Shelley.liftXPub xpub) of
        Left (ErrInvalidAddressStyle msg) -> fail msg
        Left (ErrInvalidAddressType  msg) -> fail msg
        Right addr -> B8.hPutStr stdout $ T.encodeUtf8 $ bech32 addr
