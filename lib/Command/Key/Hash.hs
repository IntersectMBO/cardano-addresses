{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Key.Hash
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( hashCredential )
import Cardano.Address.KeyHash
    ( GovernanceType, KeyHash (..), KeyRole (..), keyHashToText )
import Control.Monad
    ( when )
import Data.Maybe
    ( fromJust )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Governance
    ( governanceOpt )
import Options.Applicative.Help.Pretty
    ( pretty )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, hPutStringNoNewLn )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS
import qualified Data.Text as T

newtype Cmd = Hash
    { govType :: GovernanceType
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the hash of a public key"
        <> footerDoc (Just $ pretty $ mconcat
            [ "The public key is read from stdin and" :: Text
            , "bech32-encoded hash is returned."
            , "To get hex-encoded output pass it to stdin of `bech32`."
            ])
  where
    parser = Hash
        <$> governanceOpt

run :: Cmd -> IO ()
run Hash{govType} = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    guardBytes hrp bytes
    let keyhash = KeyHash (prefixFor hrp) (hashCredential $ BS.take 32 bytes)
    hPutStringNoNewLn stdout $ T.unpack $
        keyHashToText keyhash govType
  where
    -- Mapping of input HRP to key role
    prefixToRole =
        [ ( CIP5.addr_vk         , Payment )
        , ( CIP5.addr_xvk        , Payment )
        , ( CIP5.stake_vk        , Delegation )
        , ( CIP5.stake_xvk       , Delegation )
        , ( CIP5.addr_shared_vk  , PaymentShared )
        , ( CIP5.addr_shared_xvk , PaymentShared )
        , ( CIP5.stake_shared_vk , DelegationShared )
        , ( CIP5.stake_shared_xvk, DelegationShared )
        , ( CIP5.policy_vk       , Policy )
        , ( CIP5.policy_xvk      , Policy )
        , ( CIP5.drep_vk         , Representative )
        , ( CIP5.drep_xvk        , Representative )
        , ( CIP5.cc_cold_vk      , CommitteeCold )
        , ( CIP5.cc_cold_xvk     , CommitteeCold )
        , ( CIP5.cc_hot_vk       , CommitteeHot )
        , ( CIP5.cc_hot_xvk      , CommitteeHot )
        ]
    allowedPrefixes = map fst prefixToRole
    prefixFor = fromJust . flip lookup prefixToRole

    extendedPrefixes =
        [ CIP5.addr_xvk
        , CIP5.stake_xvk
        , CIP5.addr_shared_xvk
        , CIP5.stake_shared_xvk
        , CIP5.policy_xvk
        , CIP5.drep_xvk
        , CIP5.cc_cold_xvk
        , CIP5.cc_hot_xvk
        ]

    guardBytes hrp bytes
        | hrp `elem` extendedPrefixes = do
            when (BS.length bytes /= 64) $
                fail "data should be a 32-byte public key with a 32-byte chain-code appended"

        | otherwise = do
            when (BS.length bytes /= 32) $
                fail "data should be a 32-byte public key."
