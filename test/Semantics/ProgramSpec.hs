module Semantics.ProgramSpec (programSpec) where

import Control.Monad.Identity
import Semantics.Program
import Syntax.Tree
import State.Config as Config
import TestHelper.Denotational
import TestHelper.Output
import Test.Hspec

-- Simulates a file structure, returning the contents of a file given the file
-- path.
testTree1 :: (Monad m) => ImportPath -> m ([ImportPath], Stm)
testTree1 "File1" = return (["File2", "File3"], PrintStr "1")
testTree1 "File2" = return (["File4"], PrintStr "2")
testTree1 "File3" = return ([], PrintStr "3")
testTree1 "File4" = return ([], PrintStr "4")
testTree1 _       = error "No file"

-- Simulates a file structure, returning the contents of a file given the file
-- path. The first file overwrites the definition of a variable.
testTree2 :: ImportPath -> IO ([ImportPath], Stm)
testTree2 "File1" = return (["File2"], VarDecl "x" (Literal '1'))
testTree2 "File2" = return ([], VarDecl "x" (Literal '2'))
testTree2 _       = error "No file"

programSpec :: Spec
programSpec = do
    importStmsSpec
    evalProgSpec

importStmsSpec :: Spec
importStmsSpec = do
    describe "importStms" $ do
        it "recursively searches a tree importing files" $ do
            -- Resolving the dependencies using DFS we get the order of imports
            -- to be: File4, File2, File3, File1.
            let expected = [PrintStr "4", PrintStr "2", PrintStr "3", PrintStr "1"]
                tree = testTree1 :: ImportPath -> Identity ([ImportPath], Stm)
            importStms tree ["File1"] `shouldBe` (return expected)

        it "returns nothing if there is a cycle in the dependencies" $ do
            pending

evalProgSpec :: Spec
evalProgSpec = do
    describe "evalProg" $ do
        it "evalutes a program" $ do
            let testConfig = Config.fromString "abc"
                prog = Program [] Reject
            shouldReject "abc" $ evalProgram testTree2 prog testConfig

        it "defaults to accepting" $ do
            let testConfig = Config.fromString "abc"
                prog = Program [] MoveLeft
            shouldAccept "abc" $ evalProgram testTree2 prog testConfig

        it "works with no imports" $ do
            let testConfig = Config.fromString "abc"
                prog = Program [] (Write (Literal '1'))
            shouldAccept "1bc" $ evalProgram testTree2 prog testConfig

        -- Files 1 and Files 2 both define a constant "x", with the value '1'
        -- and '2' respectively.
        it "imports files in correct order" $ do
            let testConfig = Config.fromString ""
                prog = Program ["File1"] (Write (Var "x"))
            shouldAccept "1" $ evalProgram testTree2 prog testConfig

        it "prints" $ do
            let testConfig = Config.fromString "a"
                tree = testTree1 :: ImportPath -> TestM ([ImportPath], Stm)
                prog = Program [] PrintRead
                result = evalProgram tree prog testConfig
            result `shouldOutput` ["a"]
