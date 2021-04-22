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
            [iii|#{verKeyH1}|]

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

        specScriptNotValidated NotUniformKeyType RequiredValidation
            [iii|any [ #{verKeyH1}, #{verKeyH4}]|]

        specScriptNotValidated NotUniformKeyType RecommendedValidation
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH4} ]|]

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
verKeyH1 = "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"

verKeyH2 :: String
verKeyH2 = "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"

verKeyH3 :: String
verKeyH3 = "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"

verKeyH4 :: String
verKeyH4 = "stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5"
