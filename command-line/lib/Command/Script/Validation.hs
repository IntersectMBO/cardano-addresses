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
    ( stdout )
import System.IO.Extra
    ( hPutString, progName )

data Cmd = Cmd
    { script :: Script KeyHash
    , validationLevel :: ValidationLevel
    , txValidFrom :: Maybe Word64
    , txValidTo :: Maybe Word64
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Validate a script"
        <> header "Provide either required or recommended validation of a script given validation interval."
        <> footerDoc (Just $ vsep
            [ string "The script is taken as argument."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ progName<>" script validate 'all "
            , indent 4 $ bold $ string "[ 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe"
            , indent 4 $ bold $ string ", 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333"
            , indent 4 $ bold $ string "]'"
            , indent 2 $ string "a015ae61075e25c3d9250bdcbc35c6557272127927ecf2a2d716e29f"
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
        Left err -> hPutString stdout $ "Not validated: " <> prettyErrValidateScript err
        Right _ -> hPutString stdout "Validated."
