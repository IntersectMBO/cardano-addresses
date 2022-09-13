{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Key.FromRecoveryPhrase
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Derivation
    ( xprvToBytes )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
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
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Style
    ( Passphrase (..)
    , PassphraseInfo (..)
    , Style (..)
    , generateRootKey
    , passphraseInfoOpt
    , styleArg
    )
import System.IO
    ( stderr, stdin, stdout )
import System.IO.Extra
    ( hGetPassphraseBytes
    , hGetPassphraseMnemonic
    , hGetSomeMnemonic
    , hGetSomeMnemonicLine
    , hPutBytes
    , hPutString
    , progName
    )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5


data Cmd = FromRecoveryPhrase
    { style :: Style
    , passphraseInfo :: Maybe PassphraseInfo
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "from-recovery-phrase" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Convert a recovery phrase to an extended private key"
        <> footerDoc (Just $ vsep
            [ string "The recovery phrase is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate \\"
            , indent 2 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Icarus"
            ])
  where
    parser = FromRecoveryPhrase
        <$> styleArg
        <*> optional passphraseInfoOpt

run :: Cmd -> IO ()
run FromRecoveryPhrase{style,passphraseInfo} = do
    (someMnemonic, passphrase) <- case passphraseInfo of
        Nothing -> do
            mnemonic <- hGetSomeMnemonic stdin
            pure (mnemonic, Nothing)
        Just pinfo -> do
            hPutString stderr "Please enter a [9, 12, 15, 18, 21, 24] word mnemonic:"
            mnemonic <- hGetSomeMnemonicLine stdin
            passwd <- handlePassphraseInfo pinfo
            pure (mnemonic, Just passwd)
    rootK <- generateRootKey someMnemonic passphrase style
    hPutBytes stdout (xprvToBytes rootK) (EBech32 $ styleHrp style)
  where
    handlePassphraseInfo = \case
        Mnemonic -> do
            hPutString stderr "Please enter a 9–12 word second factor:"
            p <- hGetPassphraseMnemonic stdin
            pure $ FromMnemonic p
        Hex -> do
            hPutString stderr "Please enter hex-encoded passphrase:"
            p <- hGetPassphraseBytes stdin Hex
            pure $ FromEncoded p
        Base64 -> do
            hPutString stderr "Please enter base64-encoded passphrase:"
            p <- hGetPassphraseBytes stdin Base64
            pure $ FromEncoded p
        Utf8 -> do
            hPutString stderr "Please enter utf8-encoded passphrase:"
            p <- hGetPassphraseBytes stdin Utf8
            pure $ FromEncoded p
        Octets -> do
            hPutString stderr "Please enter passphrase in the form of octet array:"
            p <- hGetPassphraseBytes stdin Octets
            pure $ FromEncoded p

styleHrp :: Style -> HumanReadablePart
styleHrp Shared = CIP5.root_shared_xsk
styleHrp _ = CIP5.root_xsk
