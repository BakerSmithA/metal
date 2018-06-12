module Syntax.StructSpec (structSpec) where

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
    refMemberIdSpec
    structDeclSpec
    newObjSpec

refMemberIdSpec :: Spec
refMemberIdSpec = do
    describe "refMemberId" $ do
        it "accesses variables inside structures" $ do
            let state = Env.fromList [("S", PStruct [("m_x", SymType)]), ("s", PVar (CustomType "S"))]
            parseEvalState state (refMemberId SymType) "" "s.m_x" `shouldParse` ("m_x", SymType)

        it "allows chaining" $ do
            let struct1 = ("S1", PStruct [("m_s", CustomType "S2")])
                struct2 = ("S2", PStruct [("m_x", TapeType)])
                var     = ("s", PVar (CustomType "S1"))
                state   = Env.fromList [struct1, struct2, var]
            parseEvalState state (refMemberId SymType) "" "s.m_s.m_x" `shouldParse` ("m_x", TapeType)

        it "fails if any intermediate member in a chain is not a struct" $ do
            let struct1 = ("S1", PStruct [("m_s", TapeType)])
                struct2 = ("S2", PStruct [("m_x", SymType)])
                var     = ("s", PVar (CustomType "S1"))
                state   = Env.fromList [struct1, struct2, var]
            parseEvalState state (refMemberId SymType) "" "s.m_s.m_x" `shouldParse` ("m_s", TapeType)

        it "evaluates to the last access if intermediate types have the required type" $ do
            let struct = ("S", PStruct [("m_s", CustomType "S"), ("m_x", CustomType "S")])
                var    = ("s", PVar (CustomType "S"))
                state  = Env.fromList [struct, var]
            parseEvalState state (refMemberId (CustomType "S")) "" "s.m_s.m_s.m_x" `shouldParse` ("m_x", CustomType "S")

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

        it "parses recursive structures" $ do
            let expected = StructDecl "S" [("x", CustomType "S")]
            parseEmptyState program "" "struct S { x:S }" `shouldParseStm` expected

        it "fails if the struct has already been declared" $ do
            let state = Env.fromList [("S", PStruct [("x", TapeType)])]
            parseEvalState state program "" `shouldFailOn` "struct S { y:Sym }"

newObjSpec :: Spec
newObjSpec = do
    describe "newObj" $ do
        it "X" $ do
            pending
