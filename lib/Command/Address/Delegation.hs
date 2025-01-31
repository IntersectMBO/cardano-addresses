{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

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
    ( Depth (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..), ErrExtendAddress (..) )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Credential
    ( delegationCredentialArg )
import Options.Applicative.Help.Pretty
    ( Doc, annotate, bold, indent, pretty, vsep )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, progName )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

newtype Cmd = Cmd
    { credential :: Credential 'DelegationK
    } deriving Show

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "delegation" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a delegation address"
        <> footerDoc (Just $ vsep
            [ prettyText "The payment address is read from stdin."
            , prettyText ""
            , prettyText "Example:"
            , indent 2 $ annotate bold $ pretty $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat root.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key child 1852H/1815H/0H/2/0 > stake.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat root.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key child 1852H/1815H/0H/0/0 > addr.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat addr.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address payment --network-tag testnet \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address delegation $(cat stake.prv | "<>progName<>" key public --with-chain-code)"
            , indent 2 $ pretty ("addr1qpj2d4dqzds5p3mmlu95v9pex2d72cdvyjh2u3dtj4yqesv27k..." :: String)
            ])
  where
    msg = "An extended stake public key, a non-extended stake public key, a script or a script hash."
    parser = Cmd
        <$> delegationCredentialArg msg

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Cmd{credential} = do
    (_, bytes) <- hGetBech32 stdin allowedPrefixes
    case Shelley.extendAddress (unsafeMkAddress bytes) credential of
        Left (ErrInvalidAddressStyle msg) ->
            fail msg
        Left (ErrInvalidAddressType  msg) ->
            fail msg
        Left (ErrInvalidKeyHashType msg) ->
            fail msg
        Right addr ->
            B8.hPutStr stdout $ T.encodeUtf8 $ bech32 addr
  where
    allowedPrefixes =
        [ CIP5.addr
        , CIP5.addr_test
        ]
