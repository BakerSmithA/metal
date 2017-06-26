module State.ConfigSpec (configSpec) where

import State.Config
import Syntax.Tree
import Test.Hspec

configSpec :: Spec
configSpec = do
    describe "Config" $ do
        leftSpec
        rightSpec
        getCurrSpec
        setCurrSpec
        varSpec
        funcSpec

leftSpec :: Spec
leftSpec = do
    describe "left" $ do
        it "does not move the read-write head if it is zeroed" $ do
            pos (left initial) `shouldBe` 0

        it "moves the read-write head left if it is not zeroed" $ do
            let config = initial { pos = 2 }
            pos (left config) `shouldBe` 1

rightSpec :: Spec
rightSpec = do
    describe "right" $ do
        it "moves the read-write head right" $ do
            pos (right initial) `shouldBe` 1

getCurrSpec :: Spec
getCurrSpec = do
    describe "getCurr" $ do
        it "returns the symbol under the read-write head on the tape" $ do
            let config = fromString "abc"
            getCurr (config)                       `shouldBe` 'a'
            getCurr (right config)                 `shouldBe` 'b'
            getCurr (right (right config))         `shouldBe` 'c'
            getCurr (right (right (right config))) `shouldBe` ' '

setCurrSpec :: Spec
setCurrSpec = do
    describe "setCurr" $ do
        it "sets the symbol under the read-write head" $ do
            let config0 = fromString "abc"
                config1 = setCurr '1' config0
                config2 = left (setCurr '2' config1)

            getCurr config1 `shouldBe` '1'
            getCurr config2 `shouldBe` '2'

varSpec :: Spec
varSpec = do
    describe "variable environment" $ do
        it "returns Nothing if the variable is undefined" $ do
            lookupVar "x" initial `shouldBe` Nothing

        it "allows variables to be added and retrieved" $ do
            let env = addVar "x" '1' initial
            lookupVar "x" env `shouldBe` Just '1'

        it "overrides previous variable declarations" $ do
            let env  = addVar "x" '1' initial
                env' = addVar "x" '2' env
            lookupVar "x" env' `shouldBe` Just '2'

funcSpec :: Spec
funcSpec = do
    describe "function environment" $ do
        it "returns Nothing if the function is undefined" $ do
            lookupFunc "f" initial `shouldBe` Nothing

        it "allows functions to be added and retrieved" $ do
            let env = addFunc "f" MoveRight initial
            lookupFunc "f" env `shouldBe` Just MoveRight

        it "overrides previous function declarations" $ do
            let env  = addFunc "f" MoveRight initial
                env' = addFunc "f" MoveLeft env
            lookupFunc "f" env' `shouldBe` Just MoveLeft
