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
import Cardano.Address.Script
    ( KeyRole (..), keyHashAppendByteCIP0129 )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Control.Monad
    ( when )
import Data.Maybe
    ( fromJust )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( pretty )
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
        <> footerDoc (Just $ pretty $ mconcat
            [ "The public key is read from stdin and" :: Text
            , "bech32-encoded hash is returned."
            , "To get hex-encoded output pass it to stdin of `bech32`."
            ])
  where
    parser = pure Hash

run :: Cmd -> IO ()
run Hash = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    guardBytes hrp bytes
    let encoding = EBech32 $ prefixFor hrp
    hPutBytes stdout (afterHashing hrp $ hashCredential $ BS.take 32 bytes) encoding
  where
    -- Mapping of input HRP to output HRP
    prefixes =
        [ ( CIP5.addr_vk         , CIP5.addr_vkh   )
        , ( CIP5.addr_xvk        , CIP5.addr_vkh   )
        , ( CIP5.stake_vk        , CIP5.stake_vkh  )
        , ( CIP5.stake_xvk       , CIP5.stake_vkh  )
        , ( CIP5.addr_shared_vk  , CIP5.addr_shared_vkh )
        , ( CIP5.addr_shared_xvk , CIP5.addr_shared_vkh )
        , ( CIP5.stake_shared_vk , CIP5.stake_shared_vkh )
        , ( CIP5.stake_shared_xvk, CIP5.stake_shared_vkh )
        , ( CIP5.policy_vk       , CIP5.policy_vkh  )
        , ( CIP5.policy_xvk      , CIP5.policy_vkh  )
        , ( CIP5.drep_vk         , CIP5.drep  )
        , ( CIP5.drep_xvk        , CIP5.drep  )
        , ( CIP5.cc_cold_vk      , CIP5.cc_cold  )
        , ( CIP5.cc_cold_xvk     , CIP5.cc_cold  )
        , ( CIP5.cc_hot_vk       , CIP5.cc_hot  )
        , ( CIP5.cc_hot_xvk      , CIP5.cc_hot  )
        ]
    allowedPrefixes = map fst prefixes
    prefixFor = fromJust . flip lookup prefixes

    afterHashing hrp hashed
        | hrp == CIP5.drep_vk =
            keyHashAppendByteCIP0129 hashed Representative
        | hrp == CIP5.drep_xvk =
            keyHashAppendByteCIP0129 hashed Representative
        | hrp == CIP5.cc_cold_vk =
            keyHashAppendByteCIP0129 hashed CommitteeCold
        | hrp == CIP5.cc_cold_xvk =
            keyHashAppendByteCIP0129 hashed CommitteeCold
        | hrp == CIP5.cc_hot_vk =
            keyHashAppendByteCIP0129 hashed CommitteeHot
        | hrp == CIP5.cc_hot_xvk =
            keyHashAppendByteCIP0129 hashed CommitteeHot
        | otherwise =
            hashed

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
