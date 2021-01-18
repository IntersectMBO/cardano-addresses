{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Command.Script.ValidationSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( TxValidity (..), ValidationLevel (..) )
import Cardano.Address.Script.Parser
    ( ErrValidateScript (..), prettyErrValidateScript )
import Data.String.Interpolate
    ( iii )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = do
    describeCmd [ "script", "validate"] $ do
        specScriptValidated RequiredValidation (TxValidity Nothing Nothing)
            [iii|#{verKey4}|]

        specScriptValidated RequiredValidation (TxValidity Nothing Nothing)
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 10, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 6, active_until 15]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1}, #{verKeyH2}, active_from 6, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|all []|]

        specScriptNotValidated Malformed RequiredValidation (TxValidity Nothing Nothing)
            [iii|any [ #{verKeyH1}, #{verKeyH2}, active_from a]|]

        specScriptNotValidated EmptyList RecommendedValidation (TxValidity Nothing Nothing)
            [iii|all []|]

        specScriptNotValidated LedgerIncompatible RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ active_from 11, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25]|]

        specScriptNotValidated ListTooSmall RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 2 [ #{verKeyH1},  active_from 11, active_until 25, active_until 30]|]

        specScriptNotValidated RedundantTimelocks RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 1 [ #{verKeyH1},  active_from 11, active_until 25, active_until 30]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH2}]|]

        specScriptNotValidated DuplicateSignatures RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH2}]|]

        specScriptValidated RequiredValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptNotValidated MZero RecommendedValidation (TxValidity (Just 10) (Just 15))
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

levelStr :: ValidationLevel -> String
levelStr = \case
    RequiredValidation -> "--required"
    RecommendedValidation -> "--recommended"

validityStr :: TxValidity -> [String]
validityStr = \case
    TxValidity Nothing Nothing -> []
    TxValidity (Just i) Nothing -> ["--tx-valid-from",show i]
    TxValidity Nothing (Just i) -> ["--tx-valid-to",show i]
    TxValidity (Just i) (Just j) -> ["--tx-valid-from",show i,"--tx-valid-to",show j]

specScriptValidated :: ValidationLevel -> TxValidity -> String -> SpecWith ()
specScriptValidated level validity script = it (script <> " => Validated.") $ do
    out <- cli (["script", "validate", levelStr level] ++ validityStr validity ++ [script]) ""
    out `shouldBe` "Validated.\n"

specScriptNotValidated :: ErrValidateScript -> ValidationLevel -> TxValidity -> String -> SpecWith ()
specScriptNotValidated errMsg level validity script = it (script <> " => " <> show errMsg) $ do
    (out, err) <- cli (["script", "validate", levelStr level] ++ validityStr validity ++ [script]) ""
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
