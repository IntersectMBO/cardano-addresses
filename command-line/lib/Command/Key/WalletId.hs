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
import Cardano.Address.Style.Shared
    ( sharedWalletId )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Control.Monad
    ( when )
import Data.Maybe
    ( fromJust, isNothing )
import Data.Text
    ( Text )
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
import Options.Applicative.Help.Pretty
    ( pretty )
import Options.Applicative.Script
    ( scriptTemplateSpendingArg, scriptTemplateStakingArg )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS

data Cmd = WalletId
    { spending :: Maybe (Script Cosigner)
    , staking :: Maybe (Script Cosigner)
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "walletid" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Shows the cardano-wallet wallet ID for a given key"
        <> footerDoc (Just $ pretty $ mconcat
            [ "A wallet ID is a 40-digit hexadecimal string derived " :: Text
            , "from the wallet’s key. It is used by the cardano-wallet "
            , "server to refer to specific wallets.\n\n"
            , "For shelley wallets the key can be either an extended root key "
            , "(full multi-account wallets), or an extended account key "
            , "(single-account wallets).\n\n"
            , "In the latter case either private or public extended key is accepted "
            , "-- they will have the same wallet ID.\n\n"
            , "The bech32-encoded key is read from standard input.\n\n"
            , "In case of a shared wallet, the wallet id is calculated based on "
            , "private extended account key, payment template script and "
            , "staking template script - if specified. Each signature in "
            , "any template script is denoted by cosigner#number.\n"
            ])
  where
    parser = WalletId
        <$> optional scriptTemplateSpendingArg
        <*> optional scriptTemplateStakingArg

run :: Cmd -> IO ()
run WalletId{spending,staking} = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    guardBytes hrp bytes
    let bs = payloadToHash hrp bytes
    when ( hrp `elem` [CIP5.acct_shared_xvk, CIP5.acct_shared_xsk] &&
           isNothing spending ) $
        fail "shared wallet needs to have at least spending script specified"
    let walletid =
            if hrp `elem` [CIP5.acct_shared_xvk, CIP5.acct_shared_xsk] then
                sharedWalletId bs (fromJust spending) staking
            else
                hashWalletId bs
    hPutBytes stdout walletid EBase16
  where
    allowedPrefixes =
        [ CIP5.root_xsk
        , CIP5.root_xvk
        , CIP5.acct_xvk
        , CIP5.acct_xsk
        , CIP5.acct_shared_xvk
        , CIP5.acct_shared_xsk
        ]

    payloadToHash hrp bs
        | hrp `elem` [CIP5.root_xsk, CIP5.acct_xsk, CIP5.acct_shared_xsk] =
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
