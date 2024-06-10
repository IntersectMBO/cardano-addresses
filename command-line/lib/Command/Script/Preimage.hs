{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Command.Script.Preimage
    ( Cmd
    , mod
    , run

    ) where

import Prelude hiding
    ( mod )

import Cardano.Address.Script
    ( KeyHash, Script (..), serializeScript )
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
    ( stdout )
import System.IO.Extra
    ( hPutBytes, progName )

newtype Cmd = Cmd
    { script :: Script KeyHash
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "preimage" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a script preimage"
        <> header "Create a script preimage that is CBOR-encoded script."
        <> footerDoc (Just $ vsep
            [ prettyText "The script is taken as argument."
            , prettyText ""
            , prettyText "Example:"
            , indent 2 $ annotate bold $ pretty $ progName<>" script preimage 'all "
            , indent 4 $ annotate bold $ prettyText "[ addr_shared_vk1wgj79fxw2vmxkp85g88nhwlflkxevd77t6wy0nsktn2f663wdcmqcd4fp3"
            , indent 4 $ annotate bold $ prettyText ", addr_shared_vk1jthguyss2vffmszq63xsmxlpc9elxnvdyaqk7susl4sppp2s9xqsuszh44"
            , indent 4 $ annotate bold $ prettyText "]'"
            , indent 2 $ prettyText "008201828200581c1196fe3062e96b78dd959058f5adad44dd663519200f2495d17ef10d8200581c2445facc08d975d9d965d360dbe0fa63688ccc8f70fd7e1f01135380"
            , prettyText ""
            , indent 2 $ annotate bold $ pretty $ progName<>" script preimage 'all "
            , indent 4 $ annotate bold $ prettyText "[ addr_shared_vk1wgj79fxw2vmxkp85g88nhwlflkxevd77t6wy0nsktn2f663wdcmqcd4fp3"
            , indent 4 $ annotate bold $ prettyText ", active_from 100, active_until 150"
            , indent 4 $ annotate bold $ prettyText "]'"
            , indent 2 $ prettyText "008201838200581c1196fe3062e96b78dd959058f5adad44dd663519200f2495d17ef10d8204186482051896"
            ])
  where
    parser = Cmd
        <$> scriptArg

    prettyText :: Text -> Doc
    prettyText = pretty

run :: Cmd -> IO ()
run Cmd{script} = do
    hPutBytes stdout (serializeScript script) EBase16
