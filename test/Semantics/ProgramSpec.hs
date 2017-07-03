module Semantics.ProgramSpec (programSpec) where

import Semantics.Program
import System.FilePath.Posix
import Test.Hspec

programSpec :: Spec
programSpec = do
    generatePathSpec

generatePathSpec :: Spec
generatePathSpec = do
    describe "generatePath" $ do
        it "generates a file path" $ do
            let importPath = ["A", "BB", "CCC"]
                sep        = pathSeparator
                expected   = "A" ++ [sep] ++ "BB" ++ [sep] ++ "CCC"
            generatePath importPath `shouldBe` expected
