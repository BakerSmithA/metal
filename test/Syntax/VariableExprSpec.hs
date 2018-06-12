module Syntax.VariableExprSpec (variableExprSpec) where

import Syntax.Tree
import Syntax.Env as Env
import Syntax.ParseState
import Syntax.VariableExpr
import Syntax.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser

variableExprSpec :: Spec
variableExprSpec = do
    tapeSymbolSpec
    symExprSpec
    tapeExprSpec
    objExprSpec
    anyValExprSpec
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

symExprSpec :: Spec
symExprSpec = do
    describe "symExpr" $ do
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

tapeExprSpec :: Spec
tapeExprSpec = do
    describe "tapeExpr" $ do
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

objExprSpec :: Spec
objExprSpec = do
    describe "objExpr" $ do
        it "parses new objects" $ do
            let expected = NewObj "S" [(S (SymLit 'a')), (T (TapeLit "abc"))]
                state = Env.fromList [("S", PStruct [("x", SymType), ("y", TapeType)]), ("tape", PVar TapeType)]
            parseEvalState state objExpr "" "S 'a' \"abc\"" `shouldParse` expected

        it "parses new objects where arguments have parenthesis" $ do
            let expected = NewObj "S" [(S (Read (TapeVar "tape")))]
                state = Env.fromList [("S", PStruct [("x", SymType)]), ("tape", PVar TapeType)]
            parseEvalState state objExpr "" "S (read tape)" `shouldParse` expected

        it "fails if the incorrect number of arguments are given to a constructor" $ do
            let state = Env.fromList [("S", PStruct [("x", SymType), ("y", SymType)])]
            parseEvalState state objExpr "" `shouldFailOn` "S 'a'"
            parseEvalState state objExpr "" `shouldFailOn` "S 'a' \"abc\" \"abc\""

        it "fails if the incorrect type is given as an argument" $ do
            let state = Env.fromList [("S", PStruct [("x", SymType)]), ("tape", PVar TapeType)]
            parseEvalState state objExpr "" `shouldFailOn` "S tape"

        it "parses object variables" $ do
            let expected = ObjVar "S" "obj"
                state = Env.fromList [("obj", PVar (CustomType "S"))]
            parseEvalState state objExpr "" "obj" `shouldParse` expected

        it "fails if the variable is not an object" $ do
            let state = Env.fromList [("x", PVar TapeType)]
            parseEvalState state objExpr "" `shouldFailOn` "x"

anyValExprSpec :: Spec
anyValExprSpec = do
    describe "anyValExprSpec" $ do
        let state = Env.fromList [("x", PVar SymType), ("y", PVar TapeType), ("z", PVar (CustomType "S"))]

        it "parses symbols" $ do
            let expected = S (SymVar "x")
            parseEvalState state anyValExpr "" "x" `shouldParse` expected

        it "parses tapes" $ do
            let expected = T (TapeVar "y")
            parseEvalState state anyValExpr "" "y" `shouldParse` expected

        it "parses objects" $ do
            let expected = C (ObjVar "S" "z")
            parseEvalState state anyValExpr "" "z" `shouldParse` expected

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
