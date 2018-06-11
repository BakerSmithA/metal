module Semantics.ProgramSpec (programSpec) where

import Control.Monad.Identity
import Semantics.Program
import Syntax.Tree
import Syntax.Env as Env
import Test.Hspec

programSpec :: Spec
programSpec = do
    describe "program" $ do
        foldFilesSpec

foldFilesSpec :: Spec
foldFilesSpec = do
    describe "foldFiles" $ do
        it "returns accept when there are no files" $ do
            let expected = Identity Accept
            foldFiles Env.empty [] `shouldBe` expected

        it "combines parsed files" $ do
            let file1    = ("file1", "print \"Hello\"")
                file2    = ("file2", "import file1\nlet x = '1'")
                expected = Identity $ Comp (PrintStr "Hello") (Comp (VarDecl "x" (S $ SymLit '1')) Accept)
            foldFiles Env.empty [file1, file2] `shouldBe` expected

        it "allows declarations to be used by importing files" $ do
            let file1 = ("file1", "let x = '1'\nlet tape = \"abc\"")
                file2 = ("file2", "write tape x")
                decl  = Comp (VarDecl "x" (S $ SymLit '1')) (VarDecl "tape" (T $ TapeLit "abc"))
                expected = Identity $ Comp decl (Comp (Write (TapeVar "tape") (SymVar "x")) Accept)
            foldFiles Env.empty [file1, file2] `shouldBe` expected

        it "fails if used variables have not been declared" $ do
            let file = ("file1", "write tape x")
            foldFiles Env.empty [file] `shouldThrow` anyException
