{-# LANGUAGE FlexibleContexts #-}
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
    ( hashCredential, toXPub, xprvFromBytes, xpubToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Control.Monad
    ( when )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( string )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS

data Cmd = WalletId
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "walletid" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the wallet id from a given key"
        <> footerDoc (Just $ string $ mconcat
            [ "The key is read from stdin and"
            , "is assumed to be either extended root or extended account key."
            , "Both extended private and extended public keys are accepted and"
            , "the corresponding duals give rise to the same wallet id."
            , "Wallet id is 28-byte hex-encoded Blake2b hash applied on an extended public key."
            ])
  where
    parser = pure WalletId

run :: Cmd -> IO ()
run WalletId = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    guardBytes hrp bytes
    hPutBytes stdout (hashCredential $ payloadToHash hrp bytes) EBase16
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
