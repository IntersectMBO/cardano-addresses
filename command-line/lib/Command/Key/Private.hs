{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.Private
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( xprvChainCode, xprvPrivateKey )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Data.Maybe
    ( fromJust )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( pretty )
import Options.Applicative.Private
    ( PrivateType (..), privateOpt )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetXPrv, hPutBytes )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

newtype Cmd = Private
    { keyPart :: PrivateType
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "private" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Get the signing or chain code part of an extended private key"
        <> footerDoc (Just $ pretty $ mconcat
            [ "The private key is read from stdin." :: Text
            , "To get a signing key pass '--signing-key'."
            , "To get a chain code pass '--chain-code'."
            , "Bech32 encoding will be used."
            , "In order to have the signing key hex encoded pass the result to the stdin of bech32 tool."
            ])
  where
    parser = Private
        <$> privateOpt

run :: Cmd -> IO ()
run Private{keyPart} = do
    (hrp, xprv) <- hGetXPrv stdin allowedPrefixes
    let bytes = case keyPart of
            ChainCode -> xprvChainCode xprv
            SigningKey -> xprvPrivateKey xprv
    hPutBytes stdout bytes (getFormat keyPart hrp)
  where
    prefixes =
        [ (CIP5.root_xsk, CIP5.root_sk )
        , (CIP5.acct_xsk, CIP5.acct_sk )
        , (CIP5.addr_xsk, CIP5.addr_sk )
        , (CIP5.stake_xsk, CIP5.stake_sk )
        , (CIP5.root_shared_xsk, CIP5.root_shared_sk )
        , (CIP5.acct_shared_xsk, CIP5.acct_shared_sk )
        , (CIP5.addr_shared_xsk, CIP5.addr_shared_sk )
        , (CIP5.stake_shared_xsk, CIP5.stake_shared_sk )
        , (CIP5.policy_xsk, CIP5.policy_sk )
        , (CIP5.drep_xsk, CIP5.drep_sk )
        , (CIP5.cc_cold_xsk, CIP5.cc_cold_sk )
        , (CIP5.cc_hot_xsk, CIP5.cc_hot_sk )
        ]
    allowedPrefixes = map fst prefixes
    getFormat ChainCode _ = EBase16
    getFormat SigningKey hrp =
        EBech32 $ fromJust $ lookup hrp prefixes
