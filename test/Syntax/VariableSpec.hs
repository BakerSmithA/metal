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
    derivedSymbolSpec
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

derivedSymbolSpec :: Spec
derivedSymbolSpec = do
    describe "derivedSymbol" $ do
        it "parses read" $ do
            let state = Env.fromList [("tape", PVar TapeType)]
            parseEvalState state (derivedSymbol SymType) "" "read tape" `shouldParse` (Read "tape")

        it "parses symbol variables" $ do
            let state = Env.fromList [("x", PVar SymType)]
            parseEvalState state (derivedSymbol SymType) "" "x" `shouldParse` Var "x"

        it "parses tape variables" $ do
            let state = Env.fromList [("x", PVar TapeType)]
            parseEvalState state (derivedSymbol TapeType) "" "x" `shouldParse` Var "x"

        it "fails to parse variables if the types mismatch" $ do
            let state = Env.fromList [("x", PVar SymType)]
            parseEvalState state (derivedSymbol TapeType) "" `shouldFailOn` "x"

        it "parses literals" $ do
            parseEmptyState (derivedSymbol SymType) "" "'b'" `shouldParse` Literal 'b'
            parseEmptyState (derivedSymbol SymType) "" "'B'" `shouldParse` Literal 'B'
            parseEmptyState (derivedSymbol SymType) "" "'1'" `shouldParse` Literal '1'
            parseEmptyState (derivedSymbol SymType) "" "' '" `shouldParse` Literal ' '

variableDeclSpec :: Spec
variableDeclSpec = do
    describe "variable declarations" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses variable declarations" $ do
            parseEvalState state program "" "let x = read tape" `shouldParseStm` VarDecl "x" (Read "tape")

        it "fails if '=' is missing" $ do
            parseEvalState state program "" `shouldFailOn` "let x read tape"

        it "fails if a derived symbol is missing" $ do
            parseEmptyState program "" `shouldFailOn` "let x ="

        it "fails if the same variable is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "let x = 'a'\nlet x = 'b'"

        it "parses variables overwriting inside functions" $ do
            let decl     = VarDecl "x" (Literal 'x')
                func     = FuncDecl "f" [] (VarDecl "x" (Literal 'y'))
                expected = Comp decl func
            parseEmptyState program "" "let x = 'x' \n func f { let x = 'y' }" `shouldParseStm` expected

        it "resets variable environment after exiting function parse" $ do
            let s = "let x = 'x' \n func f { let x = 'y' } \n let x = 'z'"
            parseEmptyState program ""  `shouldFailOn` s

tapeDeclSpec :: Spec
tapeDeclSpec = do
    describe "tape declarations" $ do
        it "parses tape declarations" $ do
            parseEmptyState program "" "let tape = \"abcd\"" `shouldParseStm` TapeDecl "tape" "abcd"

        it "fails if the same variable is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "let x = \"abc\"\nlet x = \"xyz\""

        it "parses variables overwriting inside functions" $ do
            let decl     = TapeDecl "x" "abc"
                func     = FuncDecl "f" [] (TapeDecl "x" "xyz")
                expected = Comp decl func
            parseEmptyState program "" "let x = \"abc\" \n func f { let x = \"xyz\" }" `shouldParseStm` expected

        it "resets variable environment after exiting function parse" $ do
            let s = "let x = \"abc\" \n func f { let x = \"abc\" } \n let x = \"abc\""
            parseEmptyState program ""  `shouldFailOn` s
