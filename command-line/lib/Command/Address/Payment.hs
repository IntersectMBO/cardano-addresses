{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Payment
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( bech32With )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , MkNetworkDiscriminantError (..)
    , mkNetworkDiscriminant
    , shelleyTestnet
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Credential
    ( CredentialType (..), credentialOpt )
import Options.Applicative.Discrimination
    ( NetworkTag (..), networkTagOpt )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Style
    ( Style (..) )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetScriptHash, hGetXPub, progName )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T


data Cmd = Cmd
    {  credentialType :: CredentialType
    ,  networkTag :: NetworkTag
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "payment" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a payment address"
        <> header "Payment addresses carry no delegation rights whatsoever."
        <> footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/0/0 > addr.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat addr.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address payment --from-key --network-tag testnet"
            , indent 2 $ string "addr_test1vqrlltfahghjxl5sy5h5mvfrrlt6me5fqphhwjqvj5jd88cccqcek"
            ])
  where
    parser = Cmd
        <$> credentialOpt
        <*> networkTagOpt Shelley

run :: Cmd -> IO ()
run Cmd{networkTag,credentialType} = do
    case (mkNetworkDiscriminant . fromIntegral . unNetworkTag) networkTag of
        Left ErrWrongNetworkTag{} -> do
            fail "Invalid network tag. Must be between [0, 15]"
        Right discriminant -> do
            addr <- case credentialType of
                CredentialFromKey -> do
                    xpub <- hGetXPub stdin
                    pure $ Shelley.paymentAddress discriminant (PaymentFromKey $ Shelley.liftXPub xpub)
                CredentialFromScript -> do
                    scriptHash <- hGetScriptHash stdin
                    pure $ Shelley.paymentAddress discriminant (PaymentFromScript scriptHash)
            B8.hPutStr stdout $ T.encodeUtf8 $ bech32With hrp addr
  where
    hrp | networkTag == shelleyTestnet = [humanReadablePart|addr_test|]
        | otherwise = [humanReadablePart|addr|]
