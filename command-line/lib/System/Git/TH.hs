{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module System.Git.TH
    ( gitRevParseHEAD
    ) where

import Prelude

import Control.Exception
    ( SomeException, try )
import Language.Haskell.TH
    ( Exp (..), Lit (..), Q, runIO )
import System.Exit
    ( ExitCode (..) )
import System.Process
    ( readProcessWithExitCode )

gitRevParseHEAD :: Q Exp
gitRevParseHEAD =
    LitE . StringL <$> runIO runGitRevParse
  where
    runGitRevParse :: IO String
    runGitRevParse = do
        result <- try @SomeException $
            readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
        case result of
            Right (ExitSuccess, revision, _) -> pure revision
            _ -> pure "unknown revision"
