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
    , PassphraseInput (..)
    , PassphraseInputMode
    , Style (..)
    , fileOpt
    , generateRootKey
    , passphraseInfoOpt
    , passphraseInputModeOpt
    , styleArg
    )
import System.IO
    ( stderr, stdin, stdout )
import System.IO.Extra
    ( hGetPassphraseBytes
    , hGetPassphraseMnemonic
    , hGetSomeMnemonic
    , hGetSomeMnemonicInteractively
    , hPutBytes
    , progName
    )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5


data Cmd = FromRecoveryPhrase
    { style :: Style
    , passphraseInfo :: Maybe PassphraseInfo
    , passphraseInputMode :: PassphraseInputMode
    , passphraseFromFile :: Maybe FilePath
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "from-recovery-phrase" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Convert a recovery phrase to an extended private key"
        <> footerDoc (Just $ vsep
            [ string "The recovery phrase without passphrase is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate \\"
            , indent 2 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Icarus"
            , string ""
            , string "The recovery phrase with passphrase can be entered interactively or from file."
            , string "In both cases passhrase can take form of mnemonic, base16, base64, utf8 or octet array."
            , string "In interactive case one can select explicit, sensitive or silent mode."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" key from-recovery-phrase Shelley --passphrase from-mnemonic --sensitive"
            , indent 2 $ bold $ string "Please enter a [9, 12, 15, 18, 21, 24] word mnemonic:"
            , indent 2 $ bold $ string "**********************************************************************************************************"
            , indent 2 $ bold $ string "Please enter a 9–12 word second factor:"
            , indent 2 $ bold $ string "*************************************************************"
            , string ""
            , string "In case of passphrase reading from file the recovery phrase is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string "$ echo \"Secret Secondary Phrase\" > sndfactor.prv"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate \\"
            , indent 2 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley --from-file \"./sndfactor.prv\""
            , string ""
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 12 > sndfactor.prv"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate \\"
            , indent 2 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley --passphrase from-mnemonic --from-file \"./sndfactor.prv\""
            ])
  where
    parser = FromRecoveryPhrase
        <$> styleArg
        <*> optional passphraseInfoOpt
        <*> passphraseInputModeOpt
        <*> optional fileOpt

run :: Cmd -> IO ()
run FromRecoveryPhrase{style,passphraseInfo, passphraseInputMode,passphraseFromFile} = do
    (someMnemonic, passphrase) <- case passphraseInfo of
        Nothing -> do
            mnemonic <- hGetSomeMnemonic stdin
            pure (mnemonic, Nothing)
        Just pinfo -> do
            mnemonic <- case passphraseFromFile of
                Nothing -> do
                    let prompt = "Please enter a [9, 12, 15, 18, 21, 24] word mnemonic:"
                    hGetSomeMnemonicInteractively (stdin, stderr)
                        passphraseInputMode prompt
                Just _ ->
                    hGetSomeMnemonic stdin
            let passphraseSrc =
                    maybe Interactive FromFile passphraseFromFile
            passwd <- handlePassphraseInfo passphraseSrc pinfo
            pure (mnemonic, Just passwd)
    rootK <- generateRootKey someMnemonic passphrase style
    hPutBytes stdout (xprvToBytes rootK) (EBech32 $ styleHrp style)
  where
    handlePassphraseInfo passphraseSrc = \case
        Mnemonic -> do
            let prompt = "Please enter a 9–12 word second factor:"
            p <- hGetPassphraseMnemonic (stdin, stderr)
                 passphraseInputMode passphraseSrc prompt
            pure $ FromMnemonic p
        Hex -> do
            let prompt = "Please enter hex-encoded passphrase:"
            p <- hGetPassphraseBytes (stdin, stderr)
                 passphraseInputMode passphraseSrc prompt Hex
            pure $ FromEncoded p
        Base64 -> do
            let prompt = "Please enter base64-encoded passphrase:"
            p <- hGetPassphraseBytes (stdin, stderr)
                 passphraseInputMode passphraseSrc prompt Base64
            pure $ FromEncoded p
        Utf8 -> do
            let prompt = "Please enter utf8-encoded passphrase:"
            p <- hGetPassphraseBytes (stdin, stderr)
                 passphraseInputMode passphraseSrc prompt Utf8
            pure $ FromEncoded p
        Octets -> do
            let prompt = "Please enter passphrase in the form of octet array:"
            p <- hGetPassphraseBytes (stdin, stderr)
                 passphraseInputMode passphraseSrc prompt Octets
            pure $ FromEncoded p

styleHrp :: Style -> HumanReadablePart
styleHrp Shared = CIP5.root_shared_xsk
styleHrp _ = CIP5.root_xsk
