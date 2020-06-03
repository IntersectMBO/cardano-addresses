{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Bootstrap
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( AddressDiscrimination (..), NetworkDiscriminant, base58 )
import Cardano.Address.Style.Byron
    ( Byron )
import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , footerDoc
    , helper
    , info
    , optional
    , progDesc
    )
import Options.Applicative.Derivation
    ( DerivationPath, castDerivationPath, derivationPathOpt )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPub )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

--
-- address
--

data Cmd = Cmd
    { networkTag :: NetworkTag
    , derivationPath :: Maybe DerivationPath
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "bootstrap" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a bootstrap (i.e. legacy) address."
        <> footerDoc (Just $ string $ mconcat
            [ "Example:\n\n"
            , "  $ cardano-address recovery-phrase generate \\\n"
            , "  | cardano-address key from-recovery-phrase Byron \\\n"
            , "  | cardano-address key public \\\n"
            , "  | cardano-address address bootstrap --network-tag 764824073 --path 0H/0H\n"
            , "  DdzFFzCqrht2KG1vWt5WGhVC9Ezyu32RgB5M2DocdZ6BQU6zj69LSqksDmdM..."
            ])
  where
    parser = Cmd
        <$> networkTagOpt
        <*> optional derivationPathOpt

run :: Cmd -> IO ()
run Cmd{networkTag,derivationPath} = do
    xpub <- hGetXPub stdin
    addr <- case derivationPath of
        Nothing ->
            pure $ Icarus.paymentAddress discriminant (Icarus.liftXPub xpub)
        Just untypedPath ->
            case castDerivationPath untypedPath of
                [acctIx, addrIx] -> do
                    let path = (acctIx, toEnum (fromEnum addrIx))
                    let xkey = Byron.liftXPub path xpub
                    pure $ Byron.paymentAddress discriminant xkey
                _ -> do
                    fail "Byron derivation path must describe 2 levels (e.g. 0H/0H)"
    B8.hPutStr stdout $ T.encodeUtf8 $ base58 addr
  where
    discriminant :: NetworkDiscriminant Byron -- Or Icarus, same.
    discriminant
        | networkTag == snd Byron.byronMainnet =
            Byron.byronMainnet
        | otherwise =
            (RequiresNetworkTag, networkTag)
