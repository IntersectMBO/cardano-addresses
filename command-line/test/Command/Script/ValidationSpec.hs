{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Command.Script.ValidationSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( ErrRecommendedValidateScript (..)
    , ErrValidateScript (..)
    , ValidationLevel (..)
    , prettyErrValidateScript
    )
import Data.String.Interpolate
    ( iii )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = do
    describeCmd [ "script", "validate"] $ do
        specScriptValidated RequiredValidation
            [iii|#{verKey4}|]

        specScriptValidated RequiredValidation
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptValidated RequiredValidation
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 10, active_until 25]|]

        specScriptValidated RequiredValidation
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 6, active_until 15]|]

        specScriptValidated RequiredValidation
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 6, active_until 25]|]

        specScriptValidated RequiredValidation
            [iii|all []|]

        specScriptNotValidated Malformed RequiredValidation
            [iii|any [ #{verKeyH1}, #{verKeyH2}, active_from a]|]

        specScriptNotValidated (NotRecommended EmptyList) RecommendedValidation
            [iii|all []|]

        specScriptValidated RequiredValidation
            [iii|at_least 2 [ active_from 11, active_until 25]|]

        specScriptValidated RequiredValidation
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25]|]

        specScriptNotValidated (NotRecommended ListTooSmall) RecommendedValidation
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25]|]

        specScriptValidated RequiredValidation
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25, active_until 30]|]

        specScriptNotValidated (NotRecommended RedundantTimelocks) RecommendedValidation
            [iii|at_least 1 [ #{verKeyH1},  active_from 11, active_until 25, active_until 30]|]

        specScriptValidated RequiredValidation
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH2}]|]

        specScriptNotValidated (NotRecommended DuplicateSignatures) RecommendedValidation
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH2}]|]

        specScriptValidated RequiredValidation
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptNotValidated (NotRecommended MZero) RecommendedValidation
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

levelStr :: ValidationLevel -> String
levelStr = \case
    RequiredValidation -> "--required"
    RecommendedValidation -> "--recommended"

specScriptValidated :: ValidationLevel -> String -> SpecWith ()
specScriptValidated level script = it (script <> " => Validated.") $ do
    out <- cli (["script", "validate", levelStr level, script]) ""
    out `shouldBe` "Validated.\n"

specScriptNotValidated :: ErrValidateScript -> ValidationLevel -> String -> SpecWith ()
specScriptNotValidated errMsg level script = it (script <> " => " <> show errMsg) $ do
    (out, err) <- cli (["script", "validate", levelStr level, script]) ""
    out `shouldBe` ("" :: String)
    err `shouldContain` prettyErrValidateScript errMsg

verKeyH1 :: String
verKeyH1 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"

verKeyH2 :: String
verKeyH2 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrenxv223vj"

verKeyH3 :: String
verKeyH3 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyre5g2sfvk2"

verKey4 :: String
verKey4 = "script_vk1mjr5lrrlxuvelx94hu2cttmg5pp6cwy5h0sa37qvpcd07pv9g23skqaly0"
