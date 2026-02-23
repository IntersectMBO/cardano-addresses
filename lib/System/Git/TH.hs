{-# LANGUAGE CPP #-}

module System.Git.TH (gitRevParseHEAD) where

import Control.Monad
    ( return )
import Language.Haskell.TH
    ( Exp (..), Lit (..), Q )

gitRevParseHEAD :: Q Exp
#ifdef GITREV
gitRevParseHEAD = return (LitE (StringL GITREV))
#else
gitRevParseHEAD = return (LitE (StringL "unknown"))
#endif
