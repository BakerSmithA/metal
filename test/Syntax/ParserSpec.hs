module Syntax.ParserSpec (parserSpec) where

import Syntax.Tree
import Syntax.Env as Env
import Syntax.Parser
import Syntax.Common
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser
import Text.Megaparsec (parse)

parserSpec :: Spec
parserSpec = do
    describe "Parser" $ do
        importPathsSpec
        programSpec

importPathsSpec :: Spec
importPathsSpec =
    describe "importPaths" $ do
        it "parses just a file name" $ do
            parse importPaths "" "import FileName" `shouldParse` ["FileName"]

        it "parses a file path" $ do
            parse importPaths "" "import Dir/SubDir/FileName" `shouldParse` ["Dir/SubDir/FileName"]

        it "parses when there is a backup dir" $ do
            parse importPaths "" "import ../Dir/../FileName" `shouldParse` ["../Dir/../FileName"]

        it "parses multiple imports followed by a statement" $ do
            let expected = ["A/B", "C"]
            parse importPaths "" "import A/B\nimport C\nright tape" `shouldParse` expected

        it "parses when there are multiple newlines between imports" $ do
            let expected = ["A", "B"]
            parse importPaths "" "import A\n\nimport B\nleft" `shouldParse` expected

        context "removing whitespace and comments" $ do
            it "ignores spaces" $ do
                parse importPaths "" " import A\nleft tape" `shouldParse` ["A"]

            it "ignores newlines" $ do
                parse importPaths "" "\n\nimport A\nleft tape" `shouldParse` ["A"]

            it "ignores whole-line comments" $ do
                parse importPaths "" "//Comment\nimportA\nleft tape" `shouldParse` ["A"]

            it "ignores in-line" $ do
                parse importPaths "" "/* Comment */\nimportA\nleft tape" `shouldParse` ["A"]

programSpec :: Spec
programSpec = describe "program" $ do
    context "parsing Turing Machine operators" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses LEFT command" $ do
            parseEvalState state program "" "left tape" `shouldParseStm` (MoveLeft (Var "tape"))

        it "parses RIGHT command" $ do
            parseEvalState state program "" "right tape" `shouldParseStm` (MoveRight (Var "tape"))

        it "parses WRITE command" $ do
            parseEvalState state program "" "write tape 'x'" `shouldParseStm` (Write (Var "tape") (New $ SymLit 'x'))

        it "parses a WRITESTR command" $ do
            parseEvalState state program "" "write tape \"abcd\"" `shouldParseStm` (WriteStr (Var "tape") "abcd")

        it "parses REJECT" $ do
            parseEmptyState program "" "reject" `shouldParseStm` Reject

        it "parses ACCEPT" $ do
            parseEmptyState program "" "accept" `shouldParseStm` Accept

    context "parsing composition" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses composition" $ do
            parseEvalState state program "" "left tape\n right tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))

        it "parses composition to be right associative" $ do
            let expected = Comp (MoveLeft (Var "tape")) (Comp (MoveRight (Var "tape")) (Write (Var "tape") (New $ SymLit 'x')))
            parseEvalState state program "" "left tape \n right tape \n write tape 'x'" `shouldParseStm` expected

        it "allows for multiple newlines between statements" $ do
            parseEvalState state program "" "left tape \n\n right tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))
            parseEvalState state program "" "left tape \n\n\n right tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))

        it "fails if parsing the first statements fails to parse" $ do
            parseEvalState state program "" `shouldFailOn` "left tape \n if"

        it "fails if parsing the second statements fails to parse" $ do
            parseEvalState state program "" `shouldFailOn `"if \n left tape"

    context "parsing printing" $ do
        it "parses printing a string" $ do
            parseEmptyState program "" "print \"This is a string\"" `shouldParseStm` (PrintStr "This is a string")

        it "parses printing the symbol read from the tape" $ do
            let state = Env.fromList [("tape", PVar TapeType)]
            parseEvalState state program "" "print tape" `shouldParseStm` (PrintRead (Var "tape"))

    context "removing whitespace and comments" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        context "before statements" $ do
            it "ignores spaces" $ do
                parseEvalState state program "" " left tape" `shouldParseStm` (MoveLeft (Var "tape"))

            it "ignores newlines" $ do
                parseEvalState state program "" "\n\nleft tape" `shouldParseStm` (MoveLeft (Var "tape"))

            it "ignores whole-line comments" $ do
                parseEvalState state program "" "//Comment\n left tape" `shouldParseStm` (MoveLeft (Var "tape"))

            it "ignores in-line" $ do
                parseEvalState state program "" "/* Comment */\n left tape" `shouldParseStm` (MoveLeft (Var "tape"))

            it "ignores tabs" $ do
                parseEvalState state program "" "\tleft tape" `shouldParseStm` (MoveLeft (Var "tape"))

        context "interspersed with statements" $ do
            it "ignores whole line comments" $ do
                parseEvalState state program "" "left tape\n//Comment\n\n right tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))

            it "ignores in-line comments" $ do
                parseEvalState state program "" "if /* Comment */ True { left tape }" `shouldParseStm` (If TRUE (MoveLeft (Var "tape")) [] Nothing)

        context "ignores whitespace after of statements" $ do
            it "ignores whitespace at the end of a statement" $ do
                parseEvalState state program "" "left tape  " `shouldParseStm` (MoveLeft (Var "tape"))

            it "ignores newlines at the end of a statement" $ do
                parseEvalState state program "" "left tape\n\n" `shouldParseStm` (MoveLeft (Var "tape"))

            it "ignores tabs" $ do
                parseEvalState state program "" "left tape\t" `shouldParseStm` (MoveLeft (Var "tape"))

        context "composition" $ do
            it "ignores tabs after the newline" $ do
                parseEvalState state program "" "left tape\n\tright tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))

            it "ignores tabs before the newline" $ do
                parseEvalState state program "" "left tape\t\nright tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))

            it "ignores tabs alone on lines inbetween statements" $ do
                parseEvalState state program "" "left tape\n\t\nright tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))

            it "ignores spaces alone on lines inbetween statements" $ do
                parseEvalState state program "" "left tape\n \nright tape" `shouldParseStm` (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))

        context "whitespace nested in statements" $ do
            it "ignores newlines after an opening brace" $ do
                let expected = While TRUE (MoveLeft (Var "tape"))
                parseEvalState state program "" "while True {\n\n left tape }" `shouldParseStm` expected

            it "ignores newlines before a closing brace" $ do
                let expected = While TRUE (MoveLeft (Var "tape"))
                parseEvalState state program "" "while True { left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces" $ do
                let expected = While TRUE (MoveLeft (Var "tape"))
                parseEvalState state program "" "while True { \n\n left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines after an opening brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))
                parseEvalState state program "" "while True {\n\n left tape \n\n right tape }" `shouldParseStm` expected

            it "ignores newlines after a closing brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))
                parseEvalState state program "" "while True { left tape \n\n right tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces when composing" $ do
                let expected = While TRUE (Comp (MoveLeft (Var "tape")) (MoveRight (Var "tape")))
                parseEvalState state program "" "while True { \n\n left tape \n\n right tape \n\n }" `shouldParseStm` expected
