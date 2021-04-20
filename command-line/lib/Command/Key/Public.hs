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
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
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
    hPutBytes stdout bytes (EBech32 $ prefixToPublic chainCode hrp)
  where
    allowedPrefixes =
        [ CIP5.root_xsk
        , CIP5.acct_xsk
        , CIP5.addr_xsk
        , CIP5.stake_xsk
        , CIP5.shared_root_xsk
        , CIP5.shared_acct_xsk
        , CIP5.shared_addr_xsk
        , CIP5.shared_stake_xsk
        ]

    prefixToPublic WithChainCode hrp
        | hrp == CIP5.root_xsk = CIP5.root_xvk
        | hrp == CIP5.acct_xsk = CIP5.acct_xvk
        | hrp == CIP5.addr_xsk = CIP5.addr_xvk
        | hrp == CIP5.stake_xsk = CIP5.stake_xvk
        | hrp == CIP5.shared_root_xsk = CIP5.shared_root_xvk
        | hrp == CIP5.shared_acct_xsk = CIP5.shared_acct_xvk
        | hrp == CIP5.shared_addr_xsk = CIP5.shared_addr_xvk
        | hrp == CIP5.shared_stake_xsk = CIP5.shared_stake_xvk

    prefixToPublic WithoutChainCode hrp
        | hrp == CIP5.root_xsk = CIP5.root_vk
        | hrp == CIP5.acct_xsk = CIP5.acct_vk
        | hrp == CIP5.addr_xsk = CIP5.addr_vk
        | hrp == CIP5.stake_xsk = CIP5.stake_vk
        | hrp == CIP5.shared_root_xsk = CIP5.shared_root_vk
        | hrp == CIP5.shared_acct_xsk = CIP5.shared_acct_vk
        | hrp == CIP5.shared_addr_xsk = CIP5.shared_addr_vk
        | hrp == CIP5.shared_stake_xsk = CIP5.shared_stake_vk

    prefixToPublic _ _ =
        error "impossible: pattern-match not covering all cases."
