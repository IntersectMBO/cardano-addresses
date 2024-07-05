module Options.Applicative.Help.Pretty.Compat
    ( bold
    , string
    , module Options.Applicative.Help.Pretty
    )
    where

import Prelude

import Options.Applicative.Help.Pretty hiding
    ( bold )

import qualified Prettyprinter.Render.Terminal as ANSI

bold :: Doc -> Doc
bold = annotate ANSI.bold

string :: String -> Doc
string = pretty
