module Syntax.VariableSpec where

import Syntax.Tree
import Syntax.Env as Env
import Syntax.ParseState
import Syntax.Variable
import Syntax.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser

variableSpec :: Spec
variableSpec = do
    tapeSymbolSpec
    variableDeclSpec
    tapeDeclSpec

tapeSymbolSpec :: Spec
tapeSymbolSpec = do
    describe "tapeSymbol" $ do
        it "parses lowercase letters" $ do
            parseEmptyState tapeSymbol "" "a" `shouldParse` 'a'

        it "parses uppercase letters" $ do
            parseEmptyState tapeSymbol "" "A" `shouldParse` 'A'

        it "parses digits" $ do
            parseEmptyState tapeSymbol "" "1" `shouldParse` '1'

        it "parses a ASCII symbols" $ do
            parseEmptyState tapeSymbol "" "+" `shouldParse` '+'
            parseEmptyState tapeSymbol "" "#" `shouldParse` '#'
            parseEmptyState tapeSymbol "" "<" `shouldParse` '<'
            parseEmptyState tapeSymbol "" "{" `shouldParse` '{'

        it "does not parse single quotes" $ do
            parseEmptyState tapeSymbol "" `shouldFailOn` "\'"

variableDeclSpec :: Spec
variableDeclSpec = do
    describe "variable declarations" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses variable declarations" $ do
            let expected = VarDecl "x" (fromSymVal (Read (Var "tape")))
            parseEvalState state program "" "let x = read tape" `shouldParseStm` expected

        it "fails if '=' is missing" $ do
            parseEvalState state program "" `shouldFailOn` "let x read tape"

        it "fails if a derived symbol is missing" $ do
            parseEmptyState program "" `shouldFailOn` "let x ="

        it "fails if the same variable is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "let x = 'a'\nlet x = 'b'"

        it "parses variables overwriting inside functions" $ do
            let decl     = VarDecl "x" (fromSymVal $ SymLit 'x')
                func     = FuncDecl "f" [] (VarDecl "x" (fromSymVal $ SymLit 'y'))
                expected = Comp decl func
            parseEmptyState program "" "let x = 'x' \n func f { let x = 'y' }" `shouldParseStm` expected

        it "resets variable environment after exiting function parse" $ do
            let s = "let x = 'x' \n func f { let x = 'y' } \n let x = 'z'"
            parseEmptyState program ""  `shouldFailOn` s

tapeDeclSpec :: Spec
tapeDeclSpec = do
    describe "tape declarations" $ do
        it "parses tape declarations" $ do
            let expected = VarDecl "tape" (fromTapeVal $ TapeLit "abcd")
            parseEmptyState program "" "let tape = \"abcd\"" `shouldParseStm` expected

        it "fails if the same variable is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "let x = \"abc\"\nlet x = \"xyz\""

        it "parses variables overwriting inside functions" $ do
            let decl     = VarDecl "x" (fromTapeVal $ TapeLit "abc")
                func     = FuncDecl "f" [] (VarDecl "x" (fromTapeVal $ TapeLit "xyz"))
                expected = Comp decl func
            parseEmptyState program "" "let x = \"abc\" \n func f { let x = \"xyz\" }" `shouldParseStm` expected

        it "resets variable environment after exiting function parse" $ do
            let s = "let x = \"abc\" \n func f { let x = \"abc\" } \n let x = \"abc\""
            parseEmptyState program ""  `shouldFailOn` s
