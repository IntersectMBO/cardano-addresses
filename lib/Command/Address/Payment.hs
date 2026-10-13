{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Address.Payment
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( unAddress )
import Cardano.Address.Derivation
    ( pubFromBytes, xpubFromBytes )
import Cardano.Address.KeyHash
    ( KeyHash, KeyRole (..), keyHashFromBytes )
import Cardano.Address.Script
    ( Script, scriptHashFromBytes )
import Cardano.Address.Style.Shelley
    ( Credential (..), shelleyTestnet )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
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
import Options.Applicative.Discrimination
    ( NetworkTag (..), fromNetworkTag, networkTagOpt )
import Options.Applicative.Help.Pretty
    ( Doc, annotate, bold, indent, pretty, vsep )
import Options.Applicative.Script
    ( scriptArg )
import Options.Applicative.Style
    ( Style (..) )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32OrRaw, hPutBytes, progName )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS

data Cmd = Cmd
    { networkTag :: NetworkTag
    , paymentScript :: Maybe (Script KeyHash)
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "payment" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a payment address"
        <> header "Payment addresses carry no delegation rights whatsoever."
        <> footerDoc (Just $ vsep
            [ prettyText "Example:"
            , indent 2 $ annotate bold $ pretty $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat root.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key child 1852H/1815H/0H/0/0 > addr.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat addr.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address payment --network-tag testnet"
            , indent 2 $ prettyText "addr_test1vqrlltfahghjxl5sy5h5mvfrrlt6me5fqphhwjqvj5jd88cccqcek"
            ])
  where
    parser = Cmd
        <$> networkTagOpt Shelley
        <*> optional scriptArg

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Cmd{networkTag,paymentScript} = do
    discriminant <- fromNetworkTag networkTag
    addr <- case paymentScript of
        Just script -> do
            let credential = PaymentFromScript script
            pure $ Shelley.paymentAddress discriminant credential
        Nothing -> do
            (mHrp, bytes) <- hGetBech32OrRaw stdin allowedPrefixes
            addressFromBytes discriminant bytes mHrp
    hPutBytes stdout (unAddress addr) (EBech32 addrHrp)
  where
    addrHrp
        | networkTag == shelleyTestnet = CIP5.addr_test
        | otherwise = CIP5.addr

    allowedPrefixes =
        [ CIP5.addr_xvk
        , CIP5.addr_vk
        , CIP5.addr_vkh
        , CIP5.script
        ]

    addressFromBytes discriminant bytes mHrp
        | mHrp == Just CIP5.script = do
            case scriptHashFromBytes bytes of
                Nothing ->
                    fail "Couldn't convert bytes into script hash."
                Just h  -> do
                    let credential = PaymentFromScriptHash h
                    pure $ Shelley.paymentAddress discriminant credential

        | mHrp == Just CIP5.addr_vkh = do
            case keyHashFromBytes (Payment, bytes) of
                Nothing  ->
                    fail "Couldn't convert bytes into payment key hash."
                Just keyhash -> do
                    let credential = PaymentFromKeyHash keyhash
                    pure $ Shelley.paymentAddress discriminant credential

        | mHrp == Just CIP5.addr_vk = do
            case pubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into non-extended public key."
                Just key -> do
                    let credential = PaymentFromKey $ Shelley.liftPub key
                    pure $ Shelley.paymentAddress discriminant credential

        | mHrp == Just CIP5.addr_xvk = do
            case xpubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into extended public key."
                Just key -> do
                    let credential = PaymentFromExtendedKey $ Shelley.liftXPub key
                    pure $ Shelley.paymentAddress discriminant credential

        | otherwise = rawAddressFromBytes discriminant bytes

    -- Raw (base16/base58) input carries no prefix, so the credential kind is
    -- inferred from the byte length. A 28-byte input is ambiguous between a
    -- key hash and a script hash, hence it must be bech32-encoded.
    rawAddressFromBytes discriminant bytes
        | BS.length bytes == 64 = do
            case xpubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into extended public key."
                Just key -> do
                    let credential = PaymentFromExtendedKey $ Shelley.liftXPub key
                    pure $ Shelley.paymentAddress discriminant credential

        | BS.length bytes == 32 = do
            case pubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into non-extended public key."
                Just key -> do
                    let credential = PaymentFromKey $ Shelley.liftPub key
                    pure $ Shelley.paymentAddress discriminant credential

        | BS.length bytes == 28 =
            fail "A 28-byte payment credential is ambiguous (key hash or script \
                \hash); please provide a bech32-encoded value."

        | otherwise =
            fail "Couldn't convert bytes into a payment credential."
