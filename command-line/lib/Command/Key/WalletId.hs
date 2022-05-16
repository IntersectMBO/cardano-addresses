{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.WalletId
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( hashWalletId, toXPub, xprvFromBytes, xpubToBytes )
import Cardano.Address.Script
    ( Cosigner, Script )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Control.Monad
    ( when )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( string )
import Options.Applicative.Script
    ( scriptTemplateSpendingArg, scriptTemplateStakingArg )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS

data Cmd = WalletId
    { spending :: Script Cosigner
    , staking :: Script Cosigner
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "walletid" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Shows the cardano-wallet wallet ID for a given key"
        <> footerDoc (Just $ string $ mconcat
            [ "A wallet ID is a 40-digit hexadecimal string derived "
            , "from the wallet’s key. It is used by the cardano-wallet "
            , "server to refer to specific wallets.\n\n"
            , "For shelley wallets the key can be either an extended root key "
            , "(full multi-account wallets), or an extended account key "
            , "(single-account wallets).\n\n"
            , "In the latter case either private or public extended key is accepted"
            , "-- they will have the same wallet ID.\n\n"
            , "The bech32-encoded key is read from standard input.\n\n"
            , "In case of a shared-wallet the wallet id is calculated based on"
            , "both private (public) extended account key, payment template script and"
            , "staking template script. Each signature in any template script is denoted"
            , "by cosigner#number.\n"
            ])
  where
    parser = WalletId
        <$> scriptTemplateSpendingArg
        <*> scriptTemplateStakingArg

run :: Cmd -> IO ()
run WalletId{spending,staking} = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    guardBytes hrp bytes
    hPutBytes stdout (hashWalletId $ payloadToHash hrp bytes) EBase16
  where
    allowedPrefixes =
        [ CIP5.root_xsk
        , CIP5.root_xvk
        , CIP5.root_shared_xsk
        , CIP5.root_shared_xvk
        , CIP5.acct_xvk
        , CIP5.acct_xsk
        , CIP5.acct_shared_xvk
        , CIP5.acct_shared_xsk
        ]

    payloadToHash hrp bs
        | hrp `elem` [CIP5.root_xsk, CIP5.root_shared_xsk, CIP5.acct_xsk, CIP5.acct_shared_xsk] =
              case xprvFromBytes bs of
                  Just xprv ->  xpubToBytes . toXPub $ xprv
                  Nothing -> "96-byte extended private key is invalid due to 'scalarDecodeLong' failure from cryptonite"
        | otherwise =
              bs

    guardBytes hrp bytes
        | hrp `elem` [CIP5.root_xsk, CIP5.root_shared_xsk, CIP5.acct_xsk, CIP5.acct_shared_xsk] = do
            when (BS.length bytes /= 96) $
                fail "data should be a 96-byte private key."

        | otherwise = do
            when (BS.length bytes /= 64) $
                fail "data should be a 32-byte public key with a 32-byte chain-code appended."
