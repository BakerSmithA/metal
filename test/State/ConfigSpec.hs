module State.ConfigSpec (configSpec) where

import State.Config as Config
import qualified State.Tape as Tape hiding (Tape)
import Syntax.Tree
import Test.Hspec
import TestHelper.Config
import Data.Maybe
import Data.Map

configSpec :: Spec
configSpec = do
    describe "Config" $ do
        refSpec
        varSpec
        funcSpec
        resetEnvSpec

-- Convenience method for creating a pointer to a tape from an exisiting pointer.
copyTapePtr :: VarPath -> VarName -> Config -> Maybe Config
copyTapePtr old new c = do
    addr <- getPtr old c
    return (putPtr new addr c)

refSpec :: Spec
refSpec = do
    describe "reference (pointer) environment" $ do
        it "returns nothing if the tape is undefined" $ do
            getTapeCpy ["nothing"] Config.empty `shouldBe` Nothing

        it "allows new references to be added and retrieved" $ do
            let env = newRef "tape" (TapeRef $ Tape.fromString "abc") Config.empty
            getTapeCpy ["tape"] env `shouldBe` Just (Tape.fromString "abc")

        it "allows modification of existing references" $ do
            let env = newRef "tape" (TapeRef $ Tape.fromString "abc") Config.empty
                expected = Config.fromString "tape" "xbc"
            modifyNamedTape ["tape"] (Tape.setSym 'x') env `shouldBe` Just expected

        it "overrides previous references declarations" $ do
            let env  = newRef "tape" (TapeRef $ Tape.fromString "abc") Config.empty
                env' = newRef "tape" (TapeRef $ Tape.fromString "xyz") env
            getTapeCpy ["tape"] env' `shouldBe` Just (Tape.fromString "xyz")

        it "changed to referenced tape changes underlying tape" $ do
            let env1 = newRef "tape1" (TapeRef $ Tape.fromString "abc") Config.empty
                env2 = fromJust $ copyTapePtr ["tape1"] "tape2" env1
                env3 = fromJust $ modifyNamedTape ["tape2"] (Tape.setSym 'x') env2
            getTapeCpy ["tape1"] env3 `shouldBe` Just (Tape.fromString "xbc")

        it "fails to create a reference to another references if it does not exist" $ do
            let env = newRef "tape1" (TapeRef $ Tape.fromString "abc") Config.empty
            getPtr ["tape2"] env `shouldBe` Nothing

        it "fails if asking for a variable" $ do
            let env = putSym "x" '1' Config.empty
            getTapeCpy ["x"] env `shouldBe` Nothing

varSpec :: Spec
varSpec = do
    describe "variable environment" $ do
        it "returns Nothing if the variable is undefined" $ do
            getSym ["x"] Config.empty `shouldBe` Nothing

        it "allows variables to be added and retrieved" $ do
            let env = putSym "x" '1' Config.empty
            getSym ["x"] env `shouldBe` Just '1'

        it "overrides previous variable declarations" $ do
            let env  = putSym "x" '1' Config.empty
                env' = putSym "x" '2' env
            getSym ["x"] env' `shouldBe` Just '2'

        it "follows variable paths" $ do
            let mems = [("x", Symbol 'a')]
                env  = newRef "obj" (ObjRef $ objFromList mems) Config.empty
            getSym ["obj", "x"] env `shouldBe` Just 'a'

        it "fails to follow variable paths if the object does not have the member" $ do
            let mems = [("x", Symbol 'a')]
                env  = newRef "obj" (ObjRef $ objFromList mems) Config.empty
            getSym ["obj", "y"] env `shouldBe` Nothing

        it "fails if not asking for a symbol or ptr" $ do
            let env = newRef "tape" (TapeRef $ Tape.fromString "abc") Config.empty
            getSym ["tape"] env `shouldBe` Nothing

funcSpec :: Spec
funcSpec = do
    describe "function environment" $ do
        it "returns Nothing if the function is undefined" $ do
            getFunc "f" Config.empty `shouldBe` Nothing

        it "allows functions to be added and retrieved" $ do
            let env = putFunc "f" [] (MoveRight (TapeVar ["tape"])) Config.empty
            getFunc "f" env `shouldBe` Just ([], (MoveRight (TapeVar ["tape"])))

        it "allows functions with arguments to be added and retrieved" $ do
            let args = [("a", SymType), ("b", TapeType)]
                env = putFunc "f" args (MoveRight (TapeVar ["tape"])) Config.empty
            getFunc "f" env `shouldBe` Just (args, (MoveRight (TapeVar ["tape"])))

        it "overrides previous function declarations" $ do
            let env  = putFunc "f" [] (MoveRight (TapeVar ["tape"])) Config.empty
                env' = putFunc "f" [] (MoveLeft (TapeVar ["tape"])) env
            getFunc "f" env' `shouldBe` Just ([], (MoveLeft (TapeVar ["tape"])))

resetEnvSpec :: Spec
resetEnvSpec = do
    describe "reset env" $ do
        it "resets the symbol definitions" $ do
            let env1 = putSym "x" '1' Config.empty
                env2 = putSym "y" '2' env1
                env3 = revertEnv env1 env2

            getSym ["x"] env3 `shouldBe` Just '1'
            getSym ["y"] env3 `shouldBe` Nothing

        it "resets new reference definitions" $ do
            let env1 = newRef "x" (TapeRef $ Tape.fromString "abc") Config.empty
                env2 = newRef "y" (TapeRef $ Tape.fromString "xyz") env1
                env3 = revertEnv env1 env2

            getTapeCpy ["x"] env3 `shouldBe` Just (Tape.fromString "abc")
            getTapeCpy ["y"] env3 `shouldBe` Nothing

        it "resets references to exisiting references" $ do
            let env1 = newRef "x" (TapeRef $ Tape.fromString "abc") Config.empty
                env2 = fromJust $ copyTapePtr ["x"] "y" env1
                env3 = revertEnv env1 env2

            getTapeCpy ["x"] env3 `shouldBe` Just (Tape.fromString "abc")
            getTapeCpy ["y"] env3 `shouldBe` Nothing

        it "resets function definitions" $ do
            let env1 = putFunc "f1" [] (MoveRight (TapeVar ["tape"])) Config.empty
                env2 = putFunc "f2" [] (MoveLeft (TapeVar ["tape"])) env1
                env3 = revertEnv env1 env2

            getFunc "f1" env3 `shouldBe` Just ([], MoveRight (TapeVar ["tape"]))
            getFunc "f2" env3 `shouldBe` Nothing

        it "removes freed references" $ do
            let env1 = newRef "x" (TapeRef $ Tape.fromString "x") Config.empty
                env2 = newRef "y" (TapeRef $ Tape.fromString "y") env1
                env3 = revertEnv env1 env2

            elems (refs env3) `shouldBe` [TapeRef $ Tape.fromString "x"]

        it "adds freed addresses back to the list of free addresses" $ do
            let env1 = newRef "x" (TapeRef $ Tape.fromString "x") Config.empty
                env2 = newRef "y" (TapeRef $ Tape.fromString "y") env1
                addr = fromJust (getPtr ["y"] env2)
                env3 = revertEnv env1 env2

            (freeAddrs env3) `shouldContain` [addr]
