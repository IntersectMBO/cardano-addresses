{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Public
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( toXPub, xpubPublicKey, xpubToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Data.Maybe
    ( fromJust )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty.Compat
    ( string )
import Options.Applicative.Public
    ( PublicType (..), publicOpt )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPrv, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

newtype Cmd = Public
    { chainCode :: PublicType
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "public" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the public counterpart of a private key"
        <> footerDoc (Just $ string $ mconcat
            [ "The private key is read from stdin."
            , "To get extended public key pass '--with-chain-code'."
            , "To get public key pass '--without-chain-code'."
            ])
  where
    parser = Public
        <$> publicOpt

run :: Cmd -> IO ()
run Public{chainCode} = do
    (hrp, xprv) <- hGetXPrv stdin allowedPrefixes
    let xpub = toXPub xprv
    let bytes = case chainCode of
            WithChainCode    -> xpubToBytes xpub
            WithoutChainCode -> xpubPublicKey xpub
    hPutBytes stdout bytes (EBech32 $ prefixFor chainCode hrp)
  where
    prefixes =
        [ (CIP5.root_xsk, (CIP5.root_xvk, CIP5.root_vk) )
        , (CIP5.acct_xsk, (CIP5.acct_xvk, CIP5.acct_vk) )
        , (CIP5.addr_xsk, (CIP5.addr_xvk, CIP5.addr_vk) )
        , (CIP5.stake_xsk, (CIP5.stake_xvk, CIP5.stake_vk) )
        , (CIP5.root_shared_xsk, (CIP5.root_shared_xvk, CIP5.root_shared_vk) )
        , (CIP5.acct_shared_xsk, (CIP5.acct_shared_xvk, CIP5.acct_shared_vk) )
        , (CIP5.addr_shared_xsk, (CIP5.addr_shared_xvk, CIP5.addr_shared_vk) )
        , (CIP5.stake_shared_xsk, (CIP5.stake_shared_xvk, CIP5.stake_shared_vk) )
        , (CIP5.policy_xsk, (CIP5.policy_xvk, CIP5.policy_vk) )
        ]
    allowedPrefixes = map fst prefixes
    getCC WithChainCode = fst
    getCC WithoutChainCode = snd
    prefixFor cc = getCC cc . fromJust . flip lookup prefixes
