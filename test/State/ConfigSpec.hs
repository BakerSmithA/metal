module State.ConfigSpec (configSpec) where

import State.Config as Config
import State.Tape as Tape
import Syntax.Tree
import Test.Hspec

configSpec :: Spec
configSpec = do
    describe "Config" $ do
        tapeSpec
        varSpec
        funcSpec

tapeSpec :: Spec
tapeSpec = do
    describe "tape environment" $ do
        it "returns nothing if the tape is undefined" $ do
            getTape "nothing" Config.empty `shouldBe` Nothing

        it "allows tapes to be added and retrieved" $ do
            let env = putTape "tape" (Tape.fromString "abc") Config.empty
            getTape "tape" env `shouldBe` Just (Tape.fromString "abc")

        it "allows modification of tapes" $ do
            let env = putTape "tape" (Tape.fromString "abc") Config.empty
                expected = Config.fromString "tape" "xbc"
            modifyTape "tape" (setSym 'x') env `shouldBe` Just expected

        it "overrides previous tape declarations" $ do
            let env  = putTape "tape" (Tape.fromString "abc") Config.empty
                env' = putTape "tape" (Tape.fromString "xyz") env
            getTape "tape" env' `shouldBe` Just (Tape.fromString "xyz")

varSpec :: Spec
varSpec = do
    describe "variable environment" $ do
        it "returns Nothing if the variable is undefined" $ do
            lookupVar "x" Config.empty `shouldBe` Nothing

        it "allows variables to be added and retrieved" $ do
            let env = addVar "x" '1' Config.empty
            lookupVar "x" env `shouldBe` Just '1'

        it "overrides previous variable declarations" $ do
            let env  = addVar "x" '1' Config.empty
                env' = addVar "x" '2' env
            lookupVar "x" env' `shouldBe` Just '2'

funcSpec :: Spec
funcSpec = do
    describe "function environment" $ do
        it "returns Nothing if the function is undefined" $ do
            lookupFunc "f" Config.empty `shouldBe` Nothing

        it "allows functions to be added and retrieved" $ do
            let env = addFunc "f" [] (MoveRight "tape") Config.empty
            lookupFunc "f" env `shouldBe` Just ([], (MoveRight "tape"))

        it "allows functions with arguments to be added and retrieved" $ do
            let env = addFunc "f" ["a", "b"] (MoveRight "tape") Config.empty
            lookupFunc "f" env `shouldBe` Just (["a", "b"], (MoveRight "tape"))

        it "overrides previous function declarations" $ do
            let env  = addFunc "f" [] (MoveRight "tape") Config.empty
                env' = addFunc "f" [] (MoveLeft "tape") env
            lookupFunc "f" env' `shouldBe` Just ([], (MoveLeft "tape"))
