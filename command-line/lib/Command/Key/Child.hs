{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Child
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( DerivationScheme (..)
    , deriveXPrv
    , deriveXPub
    , indexToWord32
    , xprvPrivateKey
    , xprvToBytes
    , xpubPublicKey
    , xpubToBytes
    )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Control.Monad
    ( foldM )
import Data.Functor.Identity
    ( Identity (..) )
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
import Options.Applicative.Derivation
    ( DerivationPath, castDerivationPath, derivationPathArg )
import Options.Applicative.Help.Pretty
    ( string )
import Options.Applicative.Public
    ( PublicType (..), publicOpt )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXP__, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

data Cmd = Child
    { path :: DerivationPath
    , chainCodeM :: Maybe PublicType
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "child" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Derive child keys from a parent public/private key"
        <> footerDoc (Just $ string $ mconcat
            [ "The parent key is read from stdin."
            , "To get extended key pass '--with-chain-code' or nothing."
            , "To get non-extended key pass '--without-chain-code'."
            ])
  where
    parser = Child
        <$> derivationPathArg
        <*> optional publicOpt

run :: Cmd -> IO ()
run Child{path, chainCodeM} = do
    let cc = case chainCodeM of
                 Just WithoutChainCode -> WithoutChainCode
                 _ -> WithChainCode
    (hrp, child) <- hGetXP__ stdin allowedPrefixes >>= \case
        Left (hrp, xpub) -> do
            let ixs = castDerivationPath path
            let bytes xpub' = case chainCodeM of
                    Just WithoutChainCode -> xpubPublicKey xpub'
                    _    -> xpubToBytes xpub'
            case foldM (deriveXPub DerivationScheme2) xpub ixs of
                Just child ->
                    (,bytes child) <$> childHrpFor (indexToWord32 <$> ixs) hrp cc
                Nothing ->
                    fail
                        "Couldn't derive child key. If you're trying to derive \
                        \children on a PUBLIC key, you must use soft indexes only."

        Right (hrp, xprv) -> do
            let ixs = castDerivationPath path
            let scheme = if length ixs == 2 && hrp == CIP5.root_xsk
                    then DerivationScheme1
                    else DerivationScheme2
            let Identity child = foldM (\k -> pure . deriveXPrv scheme k) xprv ixs
            let bytes xprv' = case chainCodeM of
                    Just WithoutChainCode -> xprvPrivateKey xprv'
                    _    -> xprvToBytes xprv'
            (,bytes child) <$> childHrpFor (indexToWord32 <$> ixs) hrp WithChainCode

    hPutBytes stdout child (EBech32 hrp)
  where
    allowedPrefixes =
        [ CIP5.root_xsk
        , CIP5.acct_xsk
        , CIP5.acct_xvk
        , CIP5.root_shared_xsk
        , CIP5.acct_shared_xsk
        , CIP5.acct_shared_xvk
        ]

    -- As a reminder, we really have three scenarios:
    --
    -- Byron Legacy:
    --
    --     m / rnd_account' / rnd_address'
    --
    --
    -- Icarus & Shelley:
    --
    --     m / purpose' / coin_type' / account' / role / index
    --
    -- We do not allow derivations to anywhere in the path to avoid people
    -- shooting themselves in the foot.
    -- Hence We only allow the following transformations:
    --
    -- root_xsk => addr_xsk: (legacy)
    --     m => m / rnd_account' / rnd_address
    --
    -- root_xsk => acct_xsk: (hard derivation from root to account)
    --     m => m / purpose' / coin_type' / account'
    --
    -- root_xsk => acct_xsk: (hard derivation from root to address)
    --     m => m / purpose' / coin_type' / account' / role / index
    --
    -- acct_xsk => addr_xsk: (hard derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    -- acct_xvk => addr_xvk: (soft derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    --
    -- Shared:
    --
    --     m / purpose' / coin_type' / account' / role / index
    --
    -- purpose' = 1854H for shared wallet addresses.
    --
    -- As with Icarus/Shelley sequential wallets, to prevent undiscoverable
    -- addresses, we allow only the following transformations:
    --
    -- shared_root_xsk => shared_acct_xsk: (hard derivation from root to account)
    --     m => m / purpose' / coin_type' / account'
    --
    -- shared_root_xsk => shared_acct_xsk: (hard derivation from root to address)
    --     m => m / purpose' / coin_type' / account' / role / index
    --
    -- shared_acct_xsk => shared_addr_xsk: (hard derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    -- shared_acct_xvk => shared_addr_xvk: (soft derivation from account to address)
    --     m / purpose' / coin_type' / account' => m / purpose' / coin_type' / account' / role / index
    --
    --
    -- There's no use-case at the moment for accessing intermediate paths such
    -- as m / purpose' or m / purpose' / coin_type' so we do not expose them.
    childHrpFor [_,_,_,2,_] hrp _
        | hrp == CIP5.root_xsk = pure CIP5.stake_xsk
        | hrp == CIP5.root_shared_xsk = pure CIP5.stake_shared_xsk

    childHrpFor [_,_,_,_,_] hrp _
        | hrp == CIP5.root_xsk = pure CIP5.addr_xsk
        | hrp == CIP5.root_shared_xsk = pure CIP5.addr_shared_xsk

    childHrpFor [_,_,_] hrp _
        | hrp == CIP5.root_xsk = pure CIP5.acct_xsk
        | hrp == CIP5.root_shared_xsk = pure CIP5.acct_shared_xsk

    childHrpFor [2,_] hrp cc
        | hrp == CIP5.acct_xsk = pure CIP5.stake_xsk
        | hrp == CIP5.acct_xvk && cc == WithChainCode = pure CIP5.stake_xvk
        | hrp == CIP5.acct_xvk && cc == WithoutChainCode = pure CIP5.stake_vk
        | hrp == CIP5.acct_shared_xsk = pure CIP5.stake_shared_xsk
        | hrp == CIP5.acct_shared_xvk && cc == WithChainCode = pure CIP5.stake_shared_xvk
        | hrp == CIP5.acct_shared_xvk && cc == WithoutChainCode = pure CIP5.stake_shared_vk

    childHrpFor [_,_] hrp cc
        | hrp == CIP5.root_xsk = pure CIP5.addr_xsk
        | hrp == CIP5.acct_xsk = pure CIP5.addr_xsk
        | hrp == CIP5.acct_xvk && cc == WithChainCode = pure CIP5.addr_xvk
        | hrp == CIP5.acct_xvk && cc == WithoutChainCode = pure CIP5.addr_vk
        | hrp == CIP5.acct_shared_xsk = pure CIP5.addr_shared_xsk
        | hrp == CIP5.acct_shared_xvk && cc == WithChainCode = pure CIP5.addr_shared_xvk
        | hrp == CIP5.acct_shared_xvk && cc == WithoutChainCode = pure CIP5.addr_shared_vk

    childHrpFor _ hrp _
        | hrp == CIP5.root_xsk = fail
            "When deriving child keys from a parent root key, you must \
            \provide either 2, 3 or 5 path segments. Provide 2 (account and \
            \address) if you intend to derive a legacy Byron key. Provide 3 or 5 \
            \(purpose, coin_type, account, role, index) if you're dealing with \
            \anything else."

        | hrp == CIP5.root_shared_xsk = fail
            "When deriving child keys from a parent root key, you must \
            \provide either 3 or 5 path segments. Provide 3 \
            \(purpose, coin_type, account) or 5 \
            \(purpose, coin_type, account, role, index)."

        | otherwise = fail
            "When deriving child keys from a parent account key, you must \
            \provide exactly two path segments (role & index)."
