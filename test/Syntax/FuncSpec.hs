module Syntax.FuncSpec where

import Syntax.Tree
import Syntax.Env as Env
import Syntax.Parser
import Syntax.Common
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser

funcSpec :: Spec
funcSpec = do
    funcDeclSpec
    funcCallSpec

funcDeclSpec :: Spec
funcDeclSpec = do
    describe "function declarations" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses function delcarations" $ do
            let expected = FuncDecl "f_name" [] (MoveRight "tape")
            parseEvalState state program "" "func f_name { right tape }" `shouldParseStm` expected

        it "parses function declarations with arguments" $ do
            let args = [FuncDeclArg "a" SymType, FuncDeclArg "bb" TapeType]
                expected = FuncDecl "f_name" args (MoveRight "tape")
            parseEvalState state program "" "func f_name a:Sym bb:Tape { right tape }" `shouldParseStm` expected

        it "parses function declarations where the name contains a keyword" $ do
            let expected = FuncDecl "left_until" [] (MoveRight "tape")
            parseEvalState state program "" "func left_until { right tape }" `shouldParseStm` expected

        it "allows variables to be shadowed" $ do
            let innerVarDecl = VarDecl "x" (ValExpr $ SymLit 'a')
                func         = FuncDecl "f" [] innerVarDecl
                outerVarDecl = TapeDecl "x" (ValExpr $ TapeLit "xyz")
                comp         = Comp outerVarDecl func
            parseEvalState state program "" "let x = \"xyz\" \n func f { let x = 'a' }" `shouldParseStm` comp

        it "allows the types of variables to be changed at inner scopes" $ do
            let innerVarDecl = VarDecl "x" (ValExpr $ SymLit 'a')
                write        = Write "tape" (Var "x")
                body         = Comp innerVarDecl write
                func         = FuncDecl "f" [] body
                outerVarDecl = TapeDecl "x" (ValExpr $ TapeLit "xyz")
                comp         = Comp outerVarDecl func
            parseEvalState state program "" "let x = \"xyz\" \n func f { let x = 'a' \n write tape x }" `shouldParseStm` comp

        it "reverts variables after scope is exited" $ do
            let innerVarDecl = VarDecl "x" (ValExpr $ SymLit 'a')
                func         = FuncDecl "f" [] innerVarDecl
                outerVarDecl = TapeDecl "x" (ValExpr $ TapeLit "xyz")
                write        = Write "x" (ValExpr $ SymLit 'a')
                comp         = Comp outerVarDecl (Comp func write)
            parseEvalState state program "" "let x = \"xyz\" \n func f { let x = 'a' } \n write x 'a'" `shouldParseStm` comp

        it "allows arguments to be used inside the function" $ do
            let args = [FuncDeclArg "t" TapeType, FuncDeclArg "x" SymType]
                func = FuncDecl "write_new" args (Write "t" (Var "x"))
            parseEmptyState program "" "func write_new t:Tape x:Sym { write t x }" `shouldParseStm` func

        it "allows arguments to have the same name as variables outside" $ do
            let args = [FuncDeclArg "tape" TapeType, FuncDeclArg "x" SymType]
                func = FuncDecl "write_new" args (Write "tape" (Var "x"))
            parseEmptyState program "" "func write_new tape:Tape x:Sym { write tape x }" `shouldParseStm` func

        it "allows resursive functions" $ do
            let expected = FuncDecl "f" [] (Call "f" [])
            parseEmptyState program "" "func f { f }" `shouldParseStm` expected

        it "allows redefinition of function inside function" $ do
            let expected = FuncDecl "f" [] (FuncDecl "f" [] (MoveLeft "tape"))
            parseEvalState state program "" "func f { func f { left tape } }" `shouldParseStm` expected

        it "fails if argument names are duplicated" $ do
            parseEmptyState program "" `shouldFailOn` "func f x:Tape x:Sym { print \"\" }"

        it "fails to parse if a function name is missing" $ do
            parseEvalState state program "" `shouldFailOn` "func { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "func f_name right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "func f_name { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState state program "" `shouldFailOn` "func f_name right tape"

        it "fails if the same function is declared twice in the same scope" $ do
            parseEvalState state program "" `shouldFailOn` "func f { left tape } \n func f { left tape }"

funcCallSpec :: Spec
funcCallSpec = do
    describe "parsing function calls" $ do
        it "parses function calls" $ do
            let state = Env.fromList [("f_name", PFunc [])]
            parseEvalState state program "" "f_name" `shouldParseStm` (Call "f_name" [])

        it "parses function calls with arguments" $ do
            let expected = Call "f_name" [Var "tape", Var "x", fromSymVal (SymLit '#')]
                var1     = ("tape", PVar TapeType)
                var2     = ("x", PVar SymType)
                func     = ("f_name", PFunc [TapeType, SymType, SymType])
                state    = Env.fromList [var1, var2, func]
            parseEvalState state program "" "f_name tape x '#'" `shouldParseStm` expected

        it "parses function calls with parens around arguments" $ do
            let expected = Call "f_name" [fromSymVal (Read "tape")]
                var      = ("tape", PVar TapeType)
                func     = ("f_name", PFunc [SymType])
                state    = Env.fromList [var, func]
            parseEvalState state program "" "f_name (read tape)" `shouldParseStm` expected

        it "parses function calls with multiple spaces between arguments" $ do
            let expected = Call "f_name" [Var "tape", Var "x", fromSymVal (SymLit '#')]
                var1     = ("tape", PVar TapeType)
                var2     = ("x", PVar SymType)
                func     = ("f_name", PFunc [TapeType, SymType, SymType])
                state    = Env.fromList [var1, var2, func]
            parseEvalState state program "" "f_name   tape  x  '#'" `shouldParseStm` expected

        it "parses function calls with tabs between arguments" $ do
            let expected = Call "f_name" [Var "tape", Var "x", fromSymVal (SymLit '#')]
                var1     = ("tape", PVar TapeType)
                var2     = ("x", PVar SymType)
                func     = ("f_name", PFunc [TapeType, SymType, SymType])
                state    = Env.fromList [var1, var2, func]
            parseEvalState state program "" "f_name \ttape\tx\t'#'" `shouldParseStm` expected

        it "parses function calls followed by another statement" $ do
            let call     = Call "f_name" [Var "tape"]
                expected = Comp call ((MoveLeft "tape"))
                var      = ("tape", PVar TapeType)
                func     = ("f_name", PFunc [TapeType])
                state    = Env.fromList [var, func]
            parseEvalState state program "" "f_name tape \n left tape" `shouldParseStm` expected

        it "parses function calls where the name contains a keyword" $ do
            let expected = Call "left_until" []
                state    = Env.fromList [("left_until", PFunc [])]
            parseEvalState state program "" "left_until" `shouldParseStm` expected

        it "parses tape literal arguments" $ do
            let expected = Call "f" [fromTapeVal (TapeLit "abcd"), fromTapeVal (TapeLit "xyz")]
                state    = Env.fromList [("f", PFunc [TapeType, TapeType])]
            parseEvalState state program "" "f \"abcd\" \"xyz\"" `shouldParseStm` expected

        it "fails if function has not been declared" $ do
            parseEmptyState program "" `shouldFailOn` "f"

        it "fails if a tape is given as a symbol" $ do
            let state = Env.fromList [("x", PVar TapeType), ("f", PFunc [SymType])]
            parseEvalState state program "" `shouldFailOn` "f x"

        it "fails if a symbol is given as a tape" $ do
            let state = Env.fromList [("x", PVar SymType), ("f", PFunc [TapeType])]
            parseEvalState state program "" `shouldFailOn` "f x"

        it "fails if a symbol is given as a tape using read" $ do
            let state = Env.fromList [("x", PVar TapeType), ("f", PFunc [TapeType])]
            parseEvalState state program "" `shouldFailOn` "f read x"

        it "fails if an incorrect number of arguments are supplied" $ do
            let state = Env.fromList [("x", PVar TapeType), ("y", PVar SymType), ("f", PFunc [TapeType, SymType])]
            parseEvalState state program "" `shouldFailOn` "f"
            parseEvalState state program "" `shouldFailOn` "f x"
            parseEvalState state program "" `shouldFailOn` "f x y y"
