module Syntax.BexpSpec where

import Syntax.Tree
import Syntax.Env as Env
import Syntax.Bexp
import Syntax.Parser
import Syntax.Common
import Test.Hspec
import Test.Hspec.Megaparsec

bexpSpec :: Spec
bexpSpec = describe "bexp" $ do
    context "parsing basis elements" $ do
        it "parses True" $ do
            parseEmptyState bexp "" "True" `shouldParse` TRUE

        it "parses False" $ do
            parseEmptyState bexp "" "False" `shouldParse` FALSE

    context "parsing NOT operator" $ do
        it "parses NOT" $ do
            parseEmptyState bexp "" "not False" `shouldParse` (Not FALSE)

    context "parsing AND operator" $ do
        it "parses AND" $ do
            parseEmptyState bexp "" "True and False" `shouldParse` (And TRUE FALSE)

        it "parse to be left associative" $ do
            parseEmptyState bexp "" "True and False and False" `shouldParse` (And (And TRUE FALSE) FALSE)

    context "parsing OR operator" $ do
        it "parses OR" $ do
            parseEmptyState bexp "" "True or False" `shouldParse` (Or TRUE FALSE)

        it "parses to be left associative" $ do
            parseEmptyState bexp "" "True or False or False" `shouldParse` (Or (Or TRUE FALSE) FALSE)

    context "parsing EQ operator" $ do
        it "parses EQ" $ do
            let state = Env.fromList [("tape", PVar TapeType), ("x", PVar SymType)]
            parseEvalState state bexp "" "x == read tape" `shouldParse` (Eq (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            let state = Env.fromList [("x", PVar SymType), ("y", PVar SymType), ("z", PVar SymType)]
            parseEvalState state bexp "" "'x' == y == z" `shouldParse` (Eq (Literal 'x') (Var "y"))

    context "parsing LE operator" $ do
        it "parses LE" $ do
            let state = Env.fromList [("x", PVar SymType), ("tape", PVar TapeType)]
            parseEvalState state bexp "" "x <= read tape" `shouldParse` (Le (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            let state = Env.fromList [("x", PVar SymType), ("y", PVar SymType), ("z", PVar SymType)]
            parseEvalState state bexp "" "x <= y <= z" `shouldParse` (Le (Var "x") (Var "y"))

    context "parsing NE operator" $ do
        it "parses NE" $ do
            let state = Env.fromList [("x", PVar SymType), ("tape", PVar TapeType)]
            parseEvalState state bexp "" "x != read tape" `shouldParse` (Ne (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            let state = Env.fromList [("x", PVar SymType), ("y", PVar SymType), ("z", PVar SymType)]
            parseEvalState state bexp "" "x != y != z" `shouldParse` (Ne (Var "x") (Var "y"))

    context "parsing boolean expressions with parenthesis" $ do
        it "gives precedence to bracketed expressions" $ do
            parseEmptyState bexp "" "True and (False or False)" `shouldParse` (And TRUE (Or FALSE FALSE))

        it "allows brackets at the top level" $ do
            parseEmptyState bexp "" "(True)" `shouldParse` TRUE
