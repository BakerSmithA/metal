module Syntax.VariableSpec (variableSpec) where

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
    symSpec
    tapeSpec
    varDeclSpec

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

symSpec :: Spec
symSpec = do
    describe "sym" $ do
        it "parses reading a tape variable" $ do
            let expected = Read (TapeVar "t")
                state = Env.fromList [("t", PVar TapeType)]
            parseEvalState state symExpr "" "read t" `shouldParse` expected

        it "parses reading a tape literal" $ do
            let expected = Read (TapeLit "abc")
            parseEmptyState symExpr "" "read \"abc\"" `shouldParse` expected

        it "parses sym variables" $ do
            let expected = SymVar "x"
                state = Env.fromList [("x", PVar SymType)]
            parseEvalState state symExpr "" "x" `shouldParse` expected

        it "fails if the variable is not a symbol" $ do
            let state = Env.fromList [("t", PVar TapeType)]
            parseEvalState state symExpr "" `shouldFailOn` "t"

        it "fails if the variable is a function" $ do
            let state = Env.fromList [("f", PFunc [])]
            parseEvalState state symExpr "" `shouldFailOn` "f"

tapeSpec :: Spec
tapeSpec = do
    describe "tape" $ do
        it "parses tape literals" $ do
            let expected = TapeLit "abc"
            parseEmptyState tapeExpr "" "\"abc\"" `shouldParse` expected

        it "parses tape variables" $ do
            let expected = TapeVar "t"
                state = Env.fromList [("t", PVar TapeType)]
            parseEvalState state tapeExpr "" "t" `shouldParse` expected

        it "fails if the variable is not a tape" $ do
            let state = Env.fromList [("x", PVar SymType)]
            parseEvalState state tapeExpr "" `shouldFailOn` "x"

        it "fails reading if the variable is a function" $ do
            let state = Env.fromList [("f", PFunc [])]
            parseEvalState state tapeExpr "" `shouldFailOn` "f"

varDeclSpec :: Spec
varDeclSpec = do
    describe "variable declarations" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses variable declarations" $ do
            let expected = VarDecl "x" (S (Read (TapeVar "tape")))
            parseEvalState state program "" "let x = read tape" `shouldParseStm` expected

        it "parses variables overwriting inside functions" $ do
            let decl     = VarDecl "x" (T (TapeLit "x"))
                func     = FuncDecl "f" [] (VarDecl "x" (T (TapeLit "y")))
                expected = Comp decl func
            parseEmptyState program "" "let x = \"x\" \n func f { let x = \"y\" }" `shouldParseStm` expected

        it "resets variable environment after exiting function parse" $ do
            let s = "let x = 'x' \n func f { let x = 'y' } \n let x = 'z'"
            parseEmptyState program ""  `shouldFailOn` s

        it "infers type of a variable assigned to another variable" $ do
            let decl      = VarDecl "new_tape" (T (TapeVar "tape"))
                printRead = PrintRead (TapeVar "new_tape")
                expected  = Comp decl printRead
            parseEvalState state program "" "let new_tape = tape \n print new_tape" `shouldParse` expected

        it "fails if '=' is missing" $ do
            parseEvalState state program "" `shouldFailOn` "let x read tape"

        it "fails if a derived symbol is missing" $ do
            parseEmptyState program "" `shouldFailOn` "let x ="

        it "fails if the same variable is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "let x = 'a'\nlet x = 'b'"

        it "fails if assignment to a function" $ do
            let state' = Env.fromList [("f", PFunc [])]
            parseEvalState state' program "" `shouldFailOn` "let x = f"
