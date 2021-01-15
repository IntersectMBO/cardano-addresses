{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script.Validation
    ( Cmd
    , mod
    , run

    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Script
    ( KeyHash
    , Script (..)
    , TxValidity (..)
    , ValidationLevel (..)
    , prettyErrValidateScript
    , validateScript
    )
import Data.Word
    ( Word64 )
import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , footerDoc
    , header
    , helper
    , info
    , optional
    , progDesc
    )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import Options.Applicative.Script
    ( levelOpt, scriptArg, txValidOpt )
import System.IO
    ( stderr, stdout )
import System.IO.Extra
    ( hPutString, progName )

data Cmd = Cmd
    { script :: Script KeyHash
    , validationLevel :: ValidationLevel
    , txValidFrom :: Maybe Word64
    , txValidTo :: Maybe Word64
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "validate" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Validate a script"
        <> header "Choose a required or recommended validation of a script for a given transaction validation interval."
        <> footerDoc (Just $ vsep
            [ string "The script is taken as argument. To have required validation pass '--required'."
            , string "To have recommended validation pass '--recommended'. Recommended validation includes also"
            , string "required one. Moreover, specify transaction validity interval by specifying two slots."
            , string "Use '--tx_valid_from SLOT' and '--tx_valid_to SLOT'. Missing any one of them result in "
            , string "setting lower and upper bound, respectively."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ progName<>" script validate --required 'all"
            , indent 4 $ bold $ string "[ script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"
            , indent 4 $ bold $ string ", script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrenxv223vj"
            , indent 4 $ bold $ string "]'"
            , indent 2 $ string "Validated."
            , string ""
            , indent 2 $ bold $ string $ progName<>" script validate --recommended 'all []'"
            , indent 2 $ string "Not validated: The list inside a script is empty."
            , string ""
            , indent 2 $ bold $ string $ progName<>" script validate --required --tx-valid-from 10 --tx-valid-to 15 'at_least 1 [active_from 11, active_until 16]'"
            , indent 2 $ string "Validated."
            , string ""
            , indent 2 $ bold $ string $ progName<>" script validate --recommended --tx-valid-from 10 --tx-valid-to 15 'at_least 1 [active_from 11, active_until 16]'"
            , indent 2 $ string "Not validated: At least's number must not be larger than the non-timelock elements in the list."
            ])
  where
    parser = Cmd
        <$> scriptArg
        <*> levelOpt
        <*> optional (txValidOpt "tx-valid-from")
        <*> optional (txValidOpt "tx-valid-to")

run :: Cmd -> IO ()
run Cmd{script,validationLevel,txValidFrom,txValidTo} = do
    let txValidity = TxValidity (fromIntegral <$> txValidFrom) (fromIntegral <$> txValidTo)
    case validateScript validationLevel txValidity script of
        Left err -> hPutString stderr $ "Not validated: " <> prettyErrValidateScript err
        Right _ -> hPutString stdout "Validated."
