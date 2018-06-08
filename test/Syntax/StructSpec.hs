module Syntax.StructSpec where

import Syntax.Parser
import Syntax.Tree
import Syntax.Struct
import Syntax.ParseState
import Syntax.Env as Env
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser

structSpec :: Spec
structSpec = do
    structDeclSpec

structDeclSpec :: Spec
structDeclSpec = do
    describe "struct declaration" $ do
        it "parses with one member variable" $ do
            let expected = StructDecl "S" [("x", SymType)]
            parseEmptyState program "" "struct S { x:Sym }" `shouldParseStm` expected

        it "parses with many member variables" $ do
            let expected = StructDecl "S" [("x", SymType), ("y", TapeType)]
            parseEmptyState program "" "struct S { x:Sym \n y:Tape }" `shouldParseStm` expected

        it "allows member variables to shadow outside variables" $ do
            let expected = StructDecl "S" [("x", SymType), ("y", TapeType)]
                state    = Env.fromList [("x", PVar SymType)]
            parseEvalState state program "" "struct S { x:Sym \n y:Tape }" `shouldParseStm` expected

        it "fails if the struct has already been declared" $ do
            let state = Env.fromList [("S", PStruct [("x", TapeType)])]
            parseEvalState state program "" `shouldFailOn` "struct S { y:Sym }"
