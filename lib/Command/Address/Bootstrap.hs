{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Address.Bootstrap
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( AddressDiscrimination (..), NetworkDiscriminant, base58 )
import Cardano.Address.Derivation
    ( XPub, coerceWholeDomainIndex )
import Cardano.Address.Style.Byron
    ( Byron )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , footerDoc
    , header
    , helper
    , info
    , optional
    , progDesc
    )
import Options.Applicative.Derivation
    ( DerivationPath, castDerivationPath, derivationPathArg, xpubOpt )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Help.Pretty
    ( Doc, annotate, bold, indent, pretty, vsep )
import Options.Applicative.Style
    ( Style (..) )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPub, progName )

import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

data Cmd = Cmd
    { rootXPub :: Maybe XPub
    , networkTag :: NetworkTag
    , derivationPath :: Maybe DerivationPath
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "bootstrap" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a bootstrap address"
        <> header "Those addresses, now deprecated, were used during the Byron era."
        <> footerDoc (Just $ vsep
            [ prettyText "Example:"
            , indent 2 $ annotate bold $ pretty $ "$ "<>progName<>" recovery-phrase generate --size 12 \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key from-recovery-phrase Byron > root.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat root.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key child 14H/42H | tee addr.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address bootstrap --root $(cat root.prv | "<>progName<>" key public --with-chain-code) \\"
            , indent 8 $ annotate bold $ prettyText "--network-tag preview 14H/42H"
            , indent 2 $ prettyText "KjgoiXJS2coBYYTM69pafiau6bbGqKrzbFiRzahpWsPvit48YNiHocPpB7VJ..."
            ])
  where
    parser = Cmd
        <$> optional (xpubOpt [CIP5.root_xvk] "root" "A root public key. Needed for Byron addresses only.")
        <*> networkTagOpt Byron
        <*> optional derivationPathArg

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Cmd{networkTag,rootXPub,derivationPath} = do
    (_hrp, xpub) <- hGetXPub stdin [CIP5.addr_xvk]
    addr <- case derivationPath of
        Nothing ->
            pure $ Icarus.paymentAddress discriminant (Icarus.liftXPub xpub)
        Just untypedPath -> do
            root <- maybe
                (fail "A root public key must be provided when --path is provided") pure rootXPub
            case castDerivationPath untypedPath of
                [acctIx, addrIx] -> do
                    let path = (acctIx, coerceWholeDomainIndex addrIx)
                        xkey = Byron.liftXPub root path xpub
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
