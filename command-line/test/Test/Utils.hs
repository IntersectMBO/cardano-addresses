{-# LANGUAGE CPP #-}
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
#ifdef HJSONSCHEMA
import JSONSchema.Draft4
    ( Schema (..)
    , SchemaWithURI (..)
    , checkSchema
    , emptySchema
    , referencesViaFilesystem
    )
#endif
import System.Environment
    ( lookupEnv )
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
    cli args input = do
        (exe', args') <- getWrappedCLI args
        readProcess exe' args' input

instance CommandLine (String, String) where
    cli args input = do
        (exe', args') <- getWrappedCLI args
        dropFirst <$> readProcessWithExitCode exe' args' input
      where
        dropFirst (_,b,c) = (b, c)

exe :: String
exe = "cardano-address"

-- | Return the exe name and args for a CLI invocation.
-- For ghcjs, the cardano-address CLI must be executed under nodejs.
getWrappedCLI :: [String] -> IO (FilePath, [String])
getWrappedCLI args = maybe (exe, args) wrap <$> lookupJsExe
  where
    lookupJsExe = fmap allJs <$> lookupEnv "CARDANO_ADDRESSES_CLI"
    allJs = (<> ("/" <> exe <> ".jsexe/all.js"))
    wrap jsExe = ("node", (jsExe:args))

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

validateJSON :: SchemaRef -> Json.Value -> IO [String]
#ifdef HJSONSCHEMA
validateJSON (SchemaRef ref) value = do
    let schema = SchemaWithURI (emptySchema { _schemaRef = Just ref }) Nothing
    refs <- unsafeIO =<< referencesViaFilesystem schema
    validate <- unsafeIO (checkSchema refs schema)
    pure $ map show $ validate value
  where
    unsafeIO :: Show e => Either e a -> IO a
    unsafeIO = either (fail . show) pure
#else
validateJSON _ _ = pure []
#endif
