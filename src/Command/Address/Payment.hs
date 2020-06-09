{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Payment
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( bech32 )
import Cardano.Address.Style.Shelley
    ( MkNetworkDiscriminantError (..), mkNetworkDiscriminant )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Help.Pretty
    ( string )
import Options.Applicative.Style
    ( Style (..) )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPub )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T


newtype Cmd = Cmd
    { networkTag :: NetworkTag
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "payment" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a payment address"
        <> header "Payment addresses carry no delegation rights whatsoever."
        <> footerDoc (Just $ string $ mconcat
            [ "Example:\n"
            , "  $ cardano-address recovery-phrase generate --size 15 \\\n"
            , "  | cardano-address key from-recovery-phrase Shelley > root.prv\n"
            , "\n"
            , "  $ cat root.prv \\\n"
            , "  | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv\n"
            , "\n"
            , "  $ cat root.prv \\\n"
            , "  | cardano-address key child 1852H/1815H/0H/0/0 > addr.prv\n"
            , "\n"
            , "  $ cat addr.prv \\\n"
            , "  | cardano-address key public \\\n"
            , "  | cardano-address address payment --network-tag 0\n"
            , "\n"
            , "  addr1vrcmygdgp7v3mhz78v8kdsfru0y9wysnr9pgvvgmdqx2w0qrg8swg"
            ])
  where
    parser = Cmd
        <$> networkTagOpt Shelley

run :: Cmd -> IO ()
run Cmd{networkTag} = do
    xpub <- hGetXPub stdin
    case (mkNetworkDiscriminant . fromIntegral . unNetworkTag) networkTag of
        Left ErrWrongNetworkTag{} -> do
            fail "Invalid network tag. Must be between [0, 15]"
        Right discriminant -> do
            let addr = Shelley.paymentAddress discriminant (Shelley.liftXPub xpub)
            B8.hPutStr stdout $ T.encodeUtf8 $ bech32 addr

