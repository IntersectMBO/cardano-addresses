{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    ( unAddress )
import Cardano.Address.Derivation
    ( pubFromBytes, xpubFromBytes )
import Cardano.Address.Script
    ( KeyRole (..), keyHashFromBytes, scriptHashFromBytes )
import Cardano.Address.Style.Shelley
    ( Credential (..), shelleyTestnet )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Discrimination
    ( NetworkTag (..), fromNetworkTag, networkTagOpt )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Style
    ( Style (..) )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, hPutBytes, progName )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Codec.Bech32.Prefixes as CIP5

newtype Cmd = Cmd
    {  networkTag :: NetworkTag
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "payment" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a payment address"
        <> header "Payment addresses carry no delegation rights whatsoever."
        <> footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/0/0 > addr.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat addr.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address payment --network-tag testnet"
            , indent 2 $ string "addr_test1vqrlltfahghjxl5sy5h5mvfrrlt6me5fqphhwjqvj5jd88cccqcek"
            ])
  where
    parser = Cmd
        <$> networkTagOpt Shelley

run :: Cmd -> IO ()
run Cmd{networkTag} = do
    discriminant <- fromNetworkTag networkTag
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    addr <- addressFromBytes discriminant bytes hrp
    hPutBytes stdout (unAddress addr) (EBech32 addrHrp)
  where
    addrHrp
        | networkTag == shelleyTestnet = CIP5.addr_test
        | otherwise = CIP5.addr

    -- TODO: Also allow `XXX_vk` prefixes. We don't need the chain code to
    -- construct a payment credential. This will however need some additional
    -- abstraction over `xpubFromBytes` but I've done enough yake-shaving at
    -- this stage, so leaving this as an item for later.
    allowedPrefixes =
        [ CIP5.addr_xvk
        , CIP5.addr_vk
        , CIP5.addr_vkh
        , CIP5.script
        ]

    addressFromBytes discriminant bytes hrp
        | hrp == CIP5.script = do
            case scriptHashFromBytes bytes of
                Nothing ->
                    fail "Couldn't convert bytes into script hash."
                Just h  -> do
                    let credential = PaymentFromScriptHash h
                    pure $ Shelley.paymentAddress discriminant credential

        | hrp == CIP5.addr_vkh = do
            case keyHashFromBytes (Payment, bytes) of
                Nothing  ->
                    fail "Couldn't convert bytes into payment key hash."
                Just keyhash -> do
                    let credential = PaymentFromKeyHash keyhash
                    pure $ Shelley.paymentAddress discriminant credential

        | hrp == CIP5.addr_vk = do
            case pubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into non-extended public key."
                Just key -> do
                    let credential = PaymentFromKey $ Shelley.liftPub key
                    pure $ Shelley.paymentAddress discriminant credential

        | otherwise = do
            case xpubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into extended public key."
                Just key -> do
                    let credential = PaymentFromExtendedKey $ Shelley.liftXPub key
                    pure $ Shelley.paymentAddress discriminant credential
