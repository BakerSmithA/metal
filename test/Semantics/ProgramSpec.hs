module Semantics.ProgramSpec (programSpec) where

import Control.Monad.Identity
import Semantics.Program
import Syntax.Tree
import State.Config as Config
import TestHelper.Denotational
import Test.Hspec

-- Simulates a file structure, returning the contents of a file given the file
-- path.
testTree :: ImportPath -> Identity ([ImportPath], Stm)
testTree "File1" = return (["File2", "File3"], PrintStr "1")
testTree "File2" = return (["File4"], PrintStr "2")
testTree "File3" = return ([], PrintStr "3")
testTree "File4" = return ([], PrintStr "4")
testTree _       = error "No file"

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
            importStms testTree ["File1"] `shouldBe` (return expected)

        it "returns nothing if there is a cycle in the dependencies" $ do
            pending

evalProgSpec :: Spec
evalProgSpec = do
    describe "evalProg" $ do
        it "evalutes a program" $ do
            let testConfig = right (Config.fromString "abc")
                prog = Program [] Reject
            shouldReject "abc" $ evalProgram testTree prog testConfig

        it "defaults to accepting" $ do
            let testConfig = right (Config.fromString "abc")
                prog = Program [] MoveLeft
            shouldAccept "abc" $ evalProgram testTree prog testConfig
