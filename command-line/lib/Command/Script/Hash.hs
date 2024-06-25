{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script.Hash
    ( Cmd
    , mod
    , run

    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Script
    ( ErrValidateScript (..)
    , KeyHash (..)
    , KeyRole (..)
    , Script (..)
    , ScriptHash (..)
    , foldScript
    , prettyErrValidateScript
    , toScriptHash
    )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Data.Text
    ( Text )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, header, helper, info, progDesc )
import Options.Applicative.Help.Pretty
    ( Doc, annotate, bold, indent, pretty, vsep )
import Options.Applicative.Script
    ( scriptArg )
import System.IO
    ( stderr, stdout )
import System.IO.Extra
    ( hPutBytes, hPutString, progName )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.List as L

newtype Cmd = Cmd
    { script :: Script KeyHash
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "hash" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a script hash"
        <> header "Create a script hash that can be used in stake or payment credential in address and act as a governance credential."
        <> footerDoc (Just $ vsep
            [ prettyText "The script is taken as argument."
            , prettyText ""
            , prettyText "Example:"
            , indent 2 $ annotate bold $ pretty $ progName<>" script hash 'all "
            , indent 4 $ annotate bold $ prettyText "[ addr_shared_vk1wgj79fxw2vmxkp85g88nhwlflkxevd77t6wy0nsktn2f663wdcmqcd4fp3"
            , indent 4 $ annotate bold $ prettyText ", addr_shared_vk1jthguyss2vffmszq63xsmxlpc9elxnvdyaqk7susl4sppp2s9xqsuszh44"
            , indent 4 $ annotate bold $ prettyText "]'"
            , indent 2 $ prettyText "script1gr69m385thgvkrtspk73zmkwk537wxyxuevs2u9cukglvtlkz4k"
            ])
  where
    parser = Cmd
        <$> scriptArg

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Cmd{script} = do
    let (ScriptHash bytes) = toScriptHash script
    case checkRoles of
        Just role ->
            hPutBytes stdout bytes (EBech32 $ pickCIP5 role)
        Nothing ->
            hPutString stderr (prettyErrValidateScript NotUniformKeyType)
  where
    allKeyHashes = foldScript (:) [] script
    getRole (KeyHash r _) = r
    allRoles = map getRole allKeyHashes
    checkRoles =
        if length (L.nub allRoles) == 1 then
            Just $ head allRoles
        else
            Nothing
    pickCIP5 = \case
        Representative -> CIP5.drep_script
        CommitteeCold -> CIP5.cc_cold_script
        CommitteeHot -> CIP5.cc_hot_script
        _ -> CIP5.script
