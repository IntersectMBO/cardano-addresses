{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Address.Reward
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( NetworkTag (..), unAddress )
import Cardano.Address.Derivation
    ( pubFromBytes, xpubFromBytes )
import Cardano.Address.KeyHash
    ( KeyHash, KeyRole (..), keyHashFromBytes )
import Cardano.Address.Script
    ( Script, scriptHashFromBytes )
import Cardano.Address.Style.Shelley
    ( Credential (..), shelleyTestnet, unsafeFromRight )
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
    ( fromNetworkTag, networkTagOpt )
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
    , delegationScript :: Maybe (Script KeyHash)
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "stake" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a stake address"
        <> header "Create a stake address \
            \that references a delegation key (1-1)."
        <> footerDoc (Just $ vsep
            [ prettyText "The public key is read from stdin."
            , prettyText ""
            , prettyText "Example:"
            , indent 2 $ annotate bold $ pretty $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat root.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key child 1852H/1815H/0H/2/0 > stake.prv"
            , indent 2 $ prettyText ""
            , indent 2 $ annotate bold $ prettyText "$ cat stake.prv \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ annotate bold $ pretty $ "| "<>progName<>" address stake --network-tag testnet"
            , indent 2 $ prettyText "stake_test1uzp7swuxjx7wmpkkvat8kpgrmjl8ze0dj9lytn25qv2tm4g6n5c35"
            ])
  where
    parser = Cmd
        <$> networkTagOpt Shelley
        <*> optional scriptArg

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Cmd{networkTag,delegationScript} = do
    discriminant <- fromNetworkTag networkTag
    addr <- case delegationScript of
        Just script -> do
            let credential = DelegationFromScript script
            pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential
        Nothing -> do
            (mHrp, bytes) <- hGetBech32OrRaw stdin allowedPrefixes
            stakeAddressFromBytes discriminant bytes mHrp
    hPutBytes stdout (unAddress addr) (EBech32 stakeHrp)
  where
    stakeHrp
        | networkTag == shelleyTestnet = CIP5.stake_test
        | otherwise = CIP5.stake

    allowedPrefixes =
        [ CIP5.stake_xvk
        , CIP5.stake_vk
        , CIP5.stake_vkh
        , CIP5.script
        ]

    stakeAddressFromBytes discriminant bytes mHrp
        | mHrp == Just CIP5.script = do
            case scriptHashFromBytes bytes of
                Nothing ->
                    fail "Couldn't convert bytes into script hash."
                Just h  -> do
                    let credential = DelegationFromScriptHash h
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | mHrp == Just CIP5.stake_vkh = do
            case keyHashFromBytes (Delegation, bytes) of
                Nothing  ->
                    fail "Couldn't convert bytes into delegation key hash."
                Just keyhash -> do
                    let credential = DelegationFromKeyHash keyhash
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | mHrp == Just CIP5.stake_vk = do
            case pubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into non-extended public key."
                Just key -> do
                    let credential = DelegationFromKey $ Shelley.liftPub key
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | mHrp == Just CIP5.stake_xvk = do
            case xpubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into extended public key."
                Just key -> do
                    let credential = DelegationFromExtendedKey $ Shelley.liftXPub key
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | otherwise = rawStakeAddressFromBytes discriminant bytes

    -- Raw (base16/base58) input carries no prefix, so the credential kind is
    -- inferred from the byte length. A 28-byte input is ambiguous between a
    -- key hash and a script hash, hence it must be bech32-encoded.
    rawStakeAddressFromBytes discriminant bytes
        | BS.length bytes == 64 = do
            case xpubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into extended public key."
                Just key -> do
                    let credential = DelegationFromExtendedKey $ Shelley.liftXPub key
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | BS.length bytes == 32 = do
            case pubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into non-extended public key."
                Just key -> do
                    let credential = DelegationFromKey $ Shelley.liftPub key
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | BS.length bytes == 28 =
            fail "A 28-byte stake credential is ambiguous (key hash or script \
                \hash); please provide a bech32-encoded value."

        | otherwise =
            fail "Couldn't convert bytes into a delegation credential."
