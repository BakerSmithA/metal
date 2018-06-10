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
    valTypedSpec
    expTypeValSpec
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
            let expected = Read (Var "t")
                state = Env.fromList [("t", PVar TapeType)]
            parseEvalState state sym "" "read t" `shouldParse` expected

        it "parses reading a tape literal" $ do
            let expected = Read (New $ TapeLit "abc")
            parseEmptyState sym "" "read \"abc\"" `shouldParse` expected

        it "fails reading if the variable is not a tape" $ do
            let state = Env.fromList [("s", PVar SymType)]
            parseEvalState state sym "" `shouldFailOn` "read s"

        it "fails reading if the variable is a function" $ do
            let state = Env.fromList [("f", PFunc [])]
            parseEvalState state sym "" `shouldFailOn` "read f"

tapeSpec :: Spec
tapeSpec = do
    describe "tape" $ do
        it "parses tape literals" $ do
            let expected = TapeLit "abc"
            parseEmptyState tape "" "\"abc\"" `shouldParse` expected

valTypedSpec :: Spec
valTypedSpec = do
    describe "valTyped" $ do
        it "parses a new" $ do
            let expected = (New (TapeLit "abc"), TapeType)
            parseEmptyState (valTyped tape) "" "\"abc\"" `shouldParse` expected

        it "parses a variable" $ do
            let expected = (Var "x", TapeType)
                state = Env.fromList [("x", PVar TapeType)]
            parseEvalState state (valTyped tape) "" "x" `shouldParse` expected

expTypeValSpec :: Spec
expTypeValSpec = do
    describe "expTypeVal" $ do
        it "parses a new" $ do
            let expected = New $ SymLit 'a'
            parseEmptyState (expTypeVal SymType sym) "" "'a'" `shouldParse` expected

        it "parses a variable" $ do
            let expected = Var "x"
                state = Env.fromList [("x", PVar SymType)]
            parseEvalState state (expTypeVal SymType sym) "" "x" `shouldParse` expected

        it "fails if the variable has the incorrect type" $ do
            let state = Env.fromList [("x", PVar TapeType)]
            parseEvalState state (expTypeVal SymType sym) "" `shouldFailOn` "x"

        it "fails if the variable is a function" $ do
            let state = Env.fromList [("x", PFunc [])]
            parseEvalState state (expTypeVal SymType sym) "" `shouldFailOn` "x"

varDeclSpec :: Spec
varDeclSpec = do
    describe "variable declarations" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses variable declarations" $ do
            let expected = VarDecl "x" (fromSymVal (Read (Var "tape")))
            parseEvalState state program "" "let x = read tape" `shouldParseStm` expected

        it "parses variables overwriting inside functions" $ do
            let decl     = VarDecl "x" (fromTapeVal $ TapeLit "x")
                func     = FuncDecl "f" [] (VarDecl "x" (fromTapeVal $ TapeLit "y"))
                expected = Comp decl func
            parseEmptyState program "" "let x = \"x\" \n func f { let x = \"y\" }" `shouldParseStm` expected

        it "resets variable environment after exiting function parse" $ do
            let s = "let x = 'x' \n func f { let x = 'y' } \n let x = 'z'"
            parseEmptyState program ""  `shouldFailOn` s

        it "infers type of a variable assigned to another variable" $ do
            let decl      = VarDecl "new_tape" (Var "tape")
                printRead = PrintRead (Var "new_tape")
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
