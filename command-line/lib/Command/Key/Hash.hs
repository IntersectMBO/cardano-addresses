{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Maybe
    ( fromJust )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Format
    ( FormatType (..), formatOpt )
import Options.Applicative.Help.Pretty
    ( pretty )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString as BS

newtype Cmd = Hash
    { outputFormat :: FormatType
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the hash of a public key"
        <> footerDoc (Just $ pretty $ mconcat
            [ "The public key is read from stdin." :: Text
            , "To get hex-encoded output pass '--hex'."
            , "Otherwise bech32-encoded hash is returned."
            ])
  where
    parser = Hash
        <$> formatOpt

run :: Cmd -> IO ()
run Hash{outputFormat} = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    guardBytes hrp bytes
    let encoding = case outputFormat of
            Hex -> EBase16
            Bech32 -> EBech32 $ prefixFor hrp
    hPutBytes stdout (hashCredential $ BS.take 32 bytes) encoding
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
        ]
    allowedPrefixes = map fst prefixes
    prefixFor = fromJust . flip lookup prefixes

    guardBytes hrp bytes
        | hrp `elem` [CIP5.addr_xvk, CIP5.stake_xvk, CIP5.addr_shared_xvk, CIP5.stake_shared_xvk, CIP5.policy_xvk] = do
            when (BS.length bytes /= 64) $
                fail "data should be a 32-byte public key with a 32-byte chain-code appended"

        | otherwise = do
            when (BS.length bytes /= 32) $
                fail "data should be a 32-byte public key."
