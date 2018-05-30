module State.ConfigSpec (configSpec) where

import State.Config as Config
import qualified State.Tape as Tape
import Syntax.Tree
import Test.Hspec
import Data.Maybe

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

        it "allows new tapes to be added and retrieved" $ do
            let env = newTape "tape" (Tape.fromString "abc") Config.empty
            getTape "tape" env `shouldBe` Just (Tape.fromString "abc")

        it "allows modification of existing tapes" $ do
            let env = newTape "tape" (Tape.fromString "abc") Config.empty
                expected = Config.fromString "tape" "xbc"
            modifyTape "tape" (Tape.setSym 'x') env `shouldBe` Just expected

        it "overrides previous tape declarations" $ do
            let env  = newTape "tape" (Tape.fromString "abc") Config.empty
                env' = newTape "tape" (Tape.fromString "xyz") env
            getTape "tape" env' `shouldBe` Just (Tape.fromString "xyz")

        it "changed to referenced tape changes underlying tape" $ do
            let env1 = newTape "tape1" (Tape.fromString "abc") Config.empty
                env2 = fromJust $ putTapeRef "tape2" "tape1" env1
                env3 = fromJust $ modifyTape "tape2" (Tape.setSym 'x') env2
            getTape "tape1" env3 `shouldBe` Just (Tape.fromString "xbc")

        it "fails to create a reference to another tape if it does not exist" $ do
            let env = newTape "tape1" (Tape.fromString "abc") Config.empty
            putTapeRef "tape2" "nothing" env `shouldBe` Nothing

        it "fails if asking for a variable" $ do
            let env = putSym "x" '1' Config.empty
            getTape "x" env `shouldBe` Nothing

varSpec :: Spec
varSpec = do
    describe "variable environment" $ do
        it "returns Nothing if the variable is undefined" $ do
            getSym "x" Config.empty `shouldBe` Nothing

        it "allows variables to be added and retrieved" $ do
            let env = putSym "x" '1' Config.empty
            getSym "x" env `shouldBe` Just '1'

        it "overrides previous variable declarations" $ do
            let env  = putSym "x" '1' Config.empty
                env' = putSym "x" '2' env
            getSym "x" env' `shouldBe` Just '2'

        it "fails if asking for a tape" $ do
            let env = newTape "tape" (Tape.fromString "abc") Config.empty
            getSym "tape" env `shouldBe` Nothing

funcSpec :: Spec
funcSpec = do
    describe "function environment" $ do
        it "returns Nothing if the function is undefined" $ do
            getFunc "f" Config.empty `shouldBe` Nothing

        it "allows functions to be added and retrieved" $ do
            let env = putFunc "f" [] (MoveRight "tape") Config.empty
            getFunc "f" env `shouldBe` Just ([], (MoveRight "tape"))

        it "allows functions with arguments to be added and retrieved" $ do
            let args = [FuncDeclArg "a" SymType, FuncDeclArg "b" TapeType]
                env = putFunc "f" args (MoveRight "tape") Config.empty
            getFunc "f" env `shouldBe` Just (args, (MoveRight "tape"))

        it "overrides previous function declarations" $ do
            let env  = putFunc "f" [] (MoveRight "tape") Config.empty
                env' = putFunc "f" [] (MoveLeft "tape") env
            getFunc "f" env' `shouldBe` Just ([], (MoveLeft "tape"))
