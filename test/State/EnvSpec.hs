module State.EnvSpec (envSpec) where

import State.Env
import Syntax.Tree
import Test.Hspec

envSpec :: Spec
envSpec = do
    varSpec
    funcSpec

varSpec :: Spec
varSpec = do
    describe "variable environment" $ do
        it "returns Nothing if the variable is undefined" $ do
            lookupVar "x" empty `shouldBe` Nothing

        it "allows variables to be added and retrieved" $ do
            let env = addVar "x" '1' empty
            lookupVar "x" env `shouldBe` Just '1'

        it "overrides previous variable declarations" $ do
            let env  = addVar "x" '1' empty
                env' = addVar "x" '2' env
            lookupVar "x" env' `shouldBe` Just '2'

funcSpec :: Spec
funcSpec = do
    describe "function environment" $ do
        it "returns Nothing if the function is undefined" $ do
            lookupFunc "f" empty `shouldBe` Nothing

        it "allows functions to be added and retrieved" $ do
            let env = addFunc "f" MoveRight empty
            lookupFunc "f" env `shouldBe` Just MoveRight

        it "overrides previous variable declarations" $ do
            let env  = addFunc "f" MoveRight empty
                env' = addFunc "f" MoveLeft env
            lookupFunc "f" env' `shouldBe` Just MoveLeft
