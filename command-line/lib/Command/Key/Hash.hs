{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Hash
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( hashCredential )
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

data Cmd = Hash
    deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the hash of a public key"
        <> footerDoc (Just $ string $ mconcat
            [ "The public key is read from stdin."
            ])
  where
    parser = pure Hash

run :: Cmd -> IO ()
run Hash = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    guardBytes hrp bytes
    hPutBytes stdout (hashCredential $ BS.take 32 bytes) (EBech32 $ prefixFor hrp)
  where
    allowedPrefixes =
        [ CIP5.addr_vk
        , CIP5.addr_xvk
        , CIP5.stake_vk
        , CIP5.stake_xvk
        , CIP5.script_vk
        , CIP5.script_xvk
        ]

    guardBytes hrp bytes
        | hrp `elem` [CIP5.addr_xvk, CIP5.stake_xvk, CIP5.script_xvk] = do
            when (BS.length bytes /= 64) $
                fail "data should be a 32-byte public key with a 32-byte chain-code appended"

        | otherwise = do
            when (BS.length bytes /= 32) $
                fail "data should be a 32-byte public key."

    prefixFor hrp
        | hrp == CIP5.addr_vk   = CIP5.addr_vkh
        | hrp == CIP5.stake_vk  = CIP5.stake_vkh
        | hrp == CIP5.script_vk = CIP5.script_vkh
        | otherwise = error "impossible: pattern-match not coverage all cases."
