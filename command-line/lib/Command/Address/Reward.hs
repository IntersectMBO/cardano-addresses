{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Address.Reward
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( NetworkTag (..), bech32With )
import Cardano.Address.Style.Shelley
    ( Credential (..), mkNetworkDiscriminant, shelleyTestnet, unsafeFromRight )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Fmt
    ( build, fmt )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Credential
    ( CredentialType (..), credentialOpt )
import Options.Applicative.Discrimination
    ( networkTagOpt )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Style
    ( Style (..) )
import System.Exit
    ( die )
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
mod liftCmd = command "stake" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a stake address"
        <> header "Create a stake address \
            \that references a delegation key (1-1)."
        <> footerDoc (Just $ vsep
            [ string "The public key is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/2/0 > stake.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat stake.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address stake --network-tag testnet"
            , indent 2 $ string "stake_test1uzp7swuxjx7wmpkkvat8kpgrmjl8ze0dj9lytn25qv2tm4g6n5c35"
            ])
  where
    parser = Cmd
        <$> credentialOpt
        <*> networkTagOpt Shelley

run :: Cmd -> IO ()
run Cmd{networkTag,credentialType} = do
    case (mkNetworkDiscriminant . fromIntegral . unNetworkTag) networkTag of
        Left e -> die (fmt $ build e)
        Right discriminant -> do
            addr <- case credentialType of
                CredentialFromKey -> do
                    xpub <- hGetXPub stdin
                    let credential = DelegationFromKey $ Shelley.liftXPub xpub
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential
                CredentialFromScript -> do
                    scriptHash <- hGetScriptHash stdin
                    let credential = DelegationFromScript scriptHash
                    pure $ unsafeFromRight $ Shelley.stakeAddress discriminant credential
            B8.hPutStr stdout $ T.encodeUtf8 $ bech32With hrp addr
  where
    hrp | networkTag == shelleyTestnet = [humanReadablePart|stake_test|]
        | otherwise = [humanReadablePart|stake|]
