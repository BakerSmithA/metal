module State.ConfigSpec (configSpec) where

import State.Config as Config
import qualified State.Tape as Tape hiding (Tape)
import State.Tape (Tape)
import Syntax.Tree
import Test.Hspec
import Data.Maybe
import Data.Map

configSpec :: Spec
configSpec = do
    describe "Config" $ do
        tapeSpec
        varSpec
        funcSpec
        resetEnvSpec

-- Convenience method for creating a pointer to a tape from an exisiting pointer.
copyTapePtr :: VarName -> VarName -> Config -> Maybe Config
copyTapePtr old new c = do
    addr <- getTapePtr old c
    return (putTapePtr new addr c)

-- Convenience method for modifying the a tape with the given name.
modifyNamedTape :: VarName -> (Tape -> Tape) -> Config -> Maybe Config
modifyNamedTape name f c = modifyTape addr f c where
    addr = fromJust (getTapePtr name c)

tapeSpec :: Spec
tapeSpec = do
    describe "tape environment" $ do
        it "returns nothing if the tape is undefined" $ do
            getTapeCpy "nothing" Config.empty `shouldBe` Nothing

        it "allows new tapes to be added and retrieved" $ do
            let env = newTape "tape" (Tape.fromString "abc") Config.empty
            getTapeCpy "tape" env `shouldBe` Just (Tape.fromString "abc")

        it "allows modification of existing tapes" $ do
            let env = newTape "tape" (Tape.fromString "abc") Config.empty
                expected = Config.fromString "tape" "xbc"
            modifyNamedTape "tape" (Tape.setSym 'x') env `shouldBe` Just expected

        it "overrides previous tape declarations" $ do
            let env  = newTape "tape" (Tape.fromString "abc") Config.empty
                env' = newTape "tape" (Tape.fromString "xyz") env
            getTapeCpy "tape" env' `shouldBe` Just (Tape.fromString "xyz")

        it "changed to referenced tape changes underlying tape" $ do
            let env1 = newTape "tape1" (Tape.fromString "abc") Config.empty
                env2 = fromJust $ copyTapePtr "tape1" "tape2" env1
                env3 = fromJust $ modifyNamedTape "tape2" (Tape.setSym 'x') env2
            getTapeCpy "tape1" env3 `shouldBe` Just (Tape.fromString "xbc")

        it "fails to create a reference to another tape if it does not exist" $ do
            let env = newTape "tape1" (Tape.fromString "abc") Config.empty
            getTapePtr "tape2" env `shouldBe` Nothing

        it "fails if asking for a variable" $ do
            let env = putSym "x" '1' Config.empty
            getTapeCpy "x" env `shouldBe` Nothing

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
            let env = putFunc "f" [] (MoveRight (TapeVar "tape")) Config.empty
            getFunc "f" env `shouldBe` Just ([], (MoveRight (TapeVar "tape")))

        it "allows functions with arguments to be added and retrieved" $ do
            let args = [("a", SymType), ("b", TapeType)]
                env = putFunc "f" args (MoveRight (TapeVar "tape")) Config.empty
            getFunc "f" env `shouldBe` Just (args, (MoveRight (TapeVar "tape")))

        it "overrides previous function declarations" $ do
            let env  = putFunc "f" [] (MoveRight (TapeVar "tape")) Config.empty
                env' = putFunc "f" [] (MoveLeft (TapeVar "tape")) env
            getFunc "f" env' `shouldBe` Just ([], (MoveLeft (TapeVar "tape")))

resetEnvSpec :: Spec
resetEnvSpec = do
    describe "reset env" $ do
        it "resets the symbol definitions" $ do
            let env1 = putSym "x" '1' Config.empty
                env2 = putSym "y" '2' env1
                env3 = revertEnv env1 env2

            getSym "x" env3 `shouldBe` Just '1'
            getSym "y" env3 `shouldBe` Nothing

        it "resets new tape definitions" $ do
            let env1 = newTape "x" (Tape.fromString "abc") Config.empty
                env2 = newTape "y" (Tape.fromString "xyz") env1
                env3 = revertEnv env1 env2

            getTapeCpy "x" env3 `shouldBe` Just (Tape.fromString "abc")
            getTapeCpy "y" env3 `shouldBe` Nothing

        it "resets references to exisiting tapes" $ do
            let env1 = newTape "x" (Tape.fromString "abc") Config.empty
                env2 = fromJust $ copyTapePtr "x" "y" env1
                env3 = revertEnv env1 env2

            getTapeCpy "x" env3 `shouldBe` Just (Tape.fromString "abc")
            getTapeCpy "y" env3 `shouldBe` Nothing

        it "resets function definitions" $ do
            let env1 = putFunc "f1" [] (MoveRight (TapeVar "tape")) Config.empty
                env2 = putFunc "f2" [] (MoveLeft (TapeVar "tape")) env1
                env3 = revertEnv env1 env2

            getFunc "f1" env3 `shouldBe` Just ([], MoveRight (TapeVar "tape"))
            getFunc "f2" env3 `shouldBe` Nothing

        it "removes freed references" $ do
            let env1 = newTape "x" (Tape.fromString "x") Config.empty
                env2 = newTape "y" (Tape.fromString "y") env1
                env3 = revertEnv env1 env2

            elems (refs env3) `shouldBe` [Tape.fromString "x"]

        it "adds freed addresses back to the list of free addresses" $ do
            let env1 = newTape "x" (Tape.fromString "x") Config.empty
                env2 = newTape "y" (Tape.fromString "y") env1
                addr = fromJust (getTapePtr "y" env2)
                env3 = revertEnv env1 env2

            (freeAddrs env3) `shouldContain` [addr]
