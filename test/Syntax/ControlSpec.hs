module Syntax.ControlSpec (controlSpec) where

import Syntax.Tree
import Syntax.Env as Env
import Syntax.Parser
import Syntax.Common
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser

controlSpec :: Spec
controlSpec = do
    ifStmSpec
    whileSpec

ifStmSpec :: Spec
ifStmSpec = describe "ifStm" $ do
    let state = Env.fromList [("tape", PVar TapeType)]

    context "parsing a single IF" $ do
        it "parses IF" $ do
            let expected = (If TRUE (MoveRight (TapeVar "tape")) [] Nothing)
            parseEvalState state program "" "if True { right tape }" `shouldParseStm` expected

        it "fails to parse if a boolean expression is missing" $ do
            parseEvalState state program "" `shouldFailOn` "if { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True right tape"

    context "parsing an IF-ELSEIF" $ do
        it "parses with a single ELSE-IF clause" $ do
            let str      = "if True { right tape } else if False { left tape }"
                expected = If TRUE (MoveRight (TapeVar "tape")) [(FALSE, (MoveLeft (TapeVar "tape")))] Nothing
            parseEvalState state program "" str `shouldParseStm` expected

        it "parses with multiple ELSE-IF clauses" $ do
            let str      = "if True { right tape } else if False { left tape } else if True { accept }"
                expected = If TRUE (MoveRight (TapeVar "tape")) [(FALSE, (MoveLeft (TapeVar "tape"))), (TRUE, Accept)] Nothing
            parseEvalState state program "" str `shouldParseStm` expected

        it "fails to parse if ELSE-IF is before IF" $ do
            parseEvalState state program "" `shouldFailOn` "else if True { right tape } if True right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True { right tape } else if True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True { right tape } else if True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True { right tape } else if True right rape"

    context "parsing an ELSE clause" $ do
        it "parses ELSE with just an IF" $ do
            let str      = "if True { right tape } else { left tape }"
                expected = If TRUE (MoveRight (TapeVar "tape")) [] (Just (MoveLeft (TapeVar "tape")))
            parseEvalState state program "" str `shouldParseStm` expected

        it "parses ELSE with a preceding ELSE-IF" $ do
            let str      = "if True { right tape } else if False { left tape } else { accept }"
                expected = If TRUE (MoveRight (TapeVar "tape")) [(FALSE, (MoveLeft (TapeVar "tape")))] (Just Accept)
            parseEvalState state program "" str `shouldParseStm` expected

        it "fails to parse if the ELSE is before IF" $ do
            parseEvalState state program "" `shouldFailOn` "else { accept } if { left tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True { right tape } else right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True { right tape } else { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState state program "" `shouldFailOn` "if True { right tape } else right tape"

    context "identifier scope" $ do
        it "allows variables to be shadowed" $ do
            let innerVarDecl = VarDecl "x" (S $ SymLit 'a')
                ifStatement  = If TRUE innerVarDecl [] Nothing
                outerVarDecl = VarDecl "x" (T $ TapeLit "xyz")
                comp         = Comp outerVarDecl ifStatement
            parseEvalState state program "" "let x = \"xyz\" \n if True { let x = 'a' }" `shouldParseStm` comp

        it "allows the types of variables to be changed at inner scopes" $ do
            let innerVarDecl = VarDecl "x" (S $ SymLit 'a')
                write        = Write (TapeVar "tape") (SymVar "x")
                body         = Comp innerVarDecl write
                ifStatement  = If TRUE body [] Nothing
                outerVarDecl = VarDecl "x" (T $ TapeLit "xyz")
                comp         = Comp outerVarDecl ifStatement
            parseEvalState state program "" "let x = \"xyz\" \n if True { let x = 'a' \n write tape x }" `shouldParseStm` comp

        it "reverts variables after scope is exited" $ do
            let innerVarDecl = VarDecl "x" (S $ SymLit 'a')
                ifStatement  = If TRUE innerVarDecl [] Nothing
                outerVarDecl = VarDecl "x" (T $ TapeLit "xyz")
                write        = Write (TapeVar "x") (SymLit 'a')
                comp         = Comp outerVarDecl (Comp ifStatement write)
            parseEvalState state program "" "let x = \"xyz\" \n if True { let x = 'a' } \n write x 'a'" `shouldParseStm` comp

whileSpec :: Spec
whileSpec = do
    describe "while" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses WHILE" $ do
            let expected = (While TRUE (MoveRight (TapeVar "tape")))
            parseEvalState state program "" "while True { right tape }" `shouldParseStm` expected

        it "fails to parse if a boolean expression is missing" $ do
            parseEvalState state program "" `shouldFailOn` "while { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "while True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "while True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState state program "" `shouldFailOn` "while True right tape"

        context "identifier scope" $ do
            it "allows variables to be shadowed" $ do
                let innerVarDecl = VarDecl "x" (S $ SymLit 'a')
                    while        = While TRUE innerVarDecl
                    outerVarDecl = VarDecl "x" (T $ TapeLit "xyz")
                    comp         = Comp outerVarDecl while
                parseEvalState state program "" "let x = \"xyz\" \n while True { let x = 'a' }" `shouldParseStm` comp

            it "allows the types of variables to be changed at inner scopes" $ do
                let innerVarDecl = VarDecl "x" (S $ SymLit 'a')
                    write        = Write (TapeVar "tape") (SymVar "x")
                    body         = Comp innerVarDecl write
                    while        = While TRUE body
                    outerVarDecl = VarDecl "x" (T $ TapeLit "xyz")
                    comp         = Comp outerVarDecl while
                parseEvalState state program "" "let x = \"xyz\" \n while True { let x = 'a' \n write tape x }" `shouldParseStm` comp

            it "reverts variables after scope is exited" $ do
                let innerVarDecl = VarDecl "x" (S $ SymLit 'a')
                    while        = While TRUE innerVarDecl
                    outerVarDecl = VarDecl "x" (T $ TapeLit "xyz")
                    write        = Write (TapeVar "x") (SymLit 'a')
                    comp         = Comp outerVarDecl (Comp while write)
                parseEvalState state program "" "let x = \"xyz\" \n while True { let x = 'a' } \n write x 'a'" `shouldParseStm` comp
