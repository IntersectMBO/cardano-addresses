{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Utils
    ( cli
    , describeCmd
    , validateJSON
    , SchemaRef
    ) where

import Prelude

import Data.String
    ( IsString )
import Data.Text
    ( Text )
import JSONSchema.Draft4
    ( Schema (..)
    , SchemaWithURI (..)
    , ValidatorFailure (..)
    , checkSchema
    , emptySchema
    , referencesViaFilesystem
    )
import System.Process
    ( readProcess, readProcessWithExitCode )
import Test.Hspec
    ( Spec, SpecWith, describe, runIO )

import qualified Data.Aeson as Json


--
-- cli
--

class CommandLine output where
    cli :: [String]
           -- ^ arguments
        -> String
            -- ^ stdin
        -> IO output
            -- ^ output, either stdout or (stdout, stderr)

instance CommandLine String where
    cli = readProcess exe

instance CommandLine (String, String) where
    cli args =
        fmap dropFirst . readProcessWithExitCode exe args
      where
        dropFirst (_,b,c) = (b, c)

exe :: String
exe = "cardano-address"


--
-- describeCmd
--

-- | Wrap HSpec 'describe' into a friendly command description. So that, we get
-- a very satisfying result visually from running the tests, and can inspect
-- what each command help text looks like.
describeCmd :: [String] -> SpecWith () -> Spec
describeCmd cmd spec = do
    title <- runIO $ cli (cmd ++ ["--help"]) ""
    describe title spec


--
-- JSON Schema Validation
--

newtype SchemaRef = SchemaRef
    { getSchemaRef :: Text
    } deriving (Show, IsString)

validateJSON :: SchemaRef -> Json.Value -> IO [ValidatorFailure]
validateJSON (SchemaRef ref) value = do
    let schema = SchemaWithURI (emptySchema { _schemaRef = Just ref }) Nothing
    refs <- unsafeIO =<< referencesViaFilesystem schema
    validate <- unsafeIO (checkSchema refs schema)
    pure $ validate value
  where
    unsafeIO :: Show e => Either e a -> IO a
    unsafeIO = either (fail . show) pure
