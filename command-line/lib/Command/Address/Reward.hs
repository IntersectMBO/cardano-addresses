{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

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
import Cardano.Address.Script
    ( KeyHash, KeyRole (..), Script, keyHashFromBytes, scriptHashFromBytes )
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
    ( hGetBech32, hPutBytes, progName )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Codec.Bech32.Prefixes as CIP5

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
            (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
            stakeAddressFromBytes discriminant bytes hrp
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

    stakeAddressFromBytes discriminant bytes hrp
        | hrp == CIP5.script = do
            case scriptHashFromBytes bytes of
                Nothing ->
                    fail "Couldn't convert bytes into script hash."
                Just h  -> do
                    let credential = DelegationFromScriptHash h
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | hrp == CIP5.stake_vkh = do
            case keyHashFromBytes (Delegation, bytes) of
                Nothing  ->
                    fail "Couldn't convert bytes into delegation key hash."
                Just keyhash -> do
                    let credential = DelegationFromKeyHash keyhash
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | hrp == CIP5.stake_vk = do
            case pubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into non-extended public key."
                Just key -> do
                    let credential = DelegationFromKey $ Shelley.liftPub key
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential

        | otherwise = do
            case xpubFromBytes bytes of
                Nothing  ->
                    fail "Couldn't convert bytes into extended public key."
                Just key -> do
                    let credential = DelegationFromExtendedKey $ Shelley.liftXPub key
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential
