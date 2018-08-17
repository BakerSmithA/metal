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
        structDeclSpec
        varDeclSpec
        importPathsSpec
        programSpec

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

        it "allows newlines after the last member variable" $ do
            let expected = StructDecl "S" [("x", CustomType "S")]
            parseEmptyState program "" "struct S { x:S \n }" `shouldParseStm` expected

        it "allows other statements after a struct declaration" $ do
            let struct   = StructDecl "S" [("x", CustomType "S")]
                printSym = Print (SymLit 'a')
                comp     = Comp struct printSym
            parseEmptyState program "" "struct S { x:S }\nprint 'a'" `shouldParseStm` comp

        it "allows other statements to use the custom type" $ do
            let struct = StructDecl "S" [("x", SymType)]
                func   = FuncDecl "f" [("s", CustomType "S")] (Print (SymVar ["s", "x"]))
                comp   = Comp struct func
            parseEmptyState program "" "struct S { x:Sym }\nproc f s:S { print s.x }" `shouldParseStm` comp

        it "does not allow member access without using the dot operator" $ do
            let struct = StructDecl "S" [("x", CustomType "S")]
                func   = FuncDecl "f" [("s", CustomType "S")] (Print (SymLit 'a'))
                comp   = Comp struct func
            parseEmptyState program "" `shouldFailOn` "struct S { x:S }\nprint x"

        it "fails if the struct has already been declared" $ do
            let state = Env.fromList [("S", PStruct [("x", TapeType)])]
            parseEvalState state program "" `shouldFailOn` "struct S { y:Sym }"

varDeclSpec :: Spec
varDeclSpec = do
    describe "variable declarations" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses variable declarations" $ do
            let expected = VarDecl "x" (S (Read (TapeVar ["tape"])))
            parseEvalState state program "" "let x = read tape" `shouldParseStm` expected

        it "parses variables overwriting inside functions" $ do
            let decl     = VarDecl "x" (T (TapeLit "x"))
                func     = FuncDecl "f" [] (VarDecl "x" (T (TapeLit "y")))
                expected = Comp decl func
            parseEmptyState program "" "let x = \"x\" \n proc f { let x = \"y\" }" `shouldParseStm` expected

        it "resets variable environment after exiting function parse" $ do
            let s = "let x = 'x' \n proc f { let x = 'y' } \n let x = 'z'"
            parseEmptyState program ""  `shouldFailOn` s

        it "infers type of a variable assigned to another variable" $ do
            let decl      = VarDecl "new_tape" (T (TapeVar ["tape"]))
                printRead = Print (Read (TapeVar ["new_tape"]))
                expected  = Comp decl printRead
            parseEvalState state program "" "let new_tape = tape \n print (read new_tape)" `shouldParse` expected

        it "fails if '=' is missing" $ do
            parseEvalState state program "" `shouldFailOn` "let x read tape"

        it "fails if a derived symbol is missing" $ do
            parseEmptyState program "" `shouldFailOn` "let x ="

        it "fails if the same variable is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "let x = 'a'\nlet x = 'b'"

        it "fails if assignment to a function" $ do
            let state' = Env.fromList [("f", PFunc [])]
            parseEvalState state' program "" `shouldFailOn` "let x = f"

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

        context "LEFT command" $ do
            it "parses variables" $ do
                let expected = (MoveLeft (TapeVar ["tape"]))
                parseEvalState state program "" "left tape" `shouldParseStm` expected

            it "parses tape literals" $ do
                let expected = (MoveLeft (TapeLit "abc"))
                parseEvalState state program "" "left \"abc\"" `shouldParseStm` expected

            it "fails if given a non-tape" $ do
                let state' = Env.fromList [("x", PVar SymType)]
                parseEvalState state' program "" `shouldFailOn` "left x"

        context "parses RIGHT command" $ do
            it "parses variables" $ do
                let expected = (MoveRight (TapeVar ["tape"]))
                parseEvalState state program "" "right tape" `shouldParseStm` expected

            it "parses tape literals" $ do
                let expected = (MoveRight (TapeLit "abc"))
                parseEvalState state program "" "right \"abc\"" `shouldParseStm` expected

            it "fails if given a non-tape" $ do
                let state' = Env.fromList [("x", PVar SymType)]
                parseEvalState state' program "" `shouldFailOn` "right x"

        context "parses WRITE command" $ do
            it "parses variables" $ do
                let expected = (Write (TapeVar ["tape"]) (SymLit 'x'))
                parseEvalState state program "" "write tape 'x'" `shouldParseStm` expected

            it "parses tape literals" $ do
                let expected = (Write (TapeLit "abc") (SymLit 'x'))
                parseEvalState state program "" "write \"abc\" 'x'" `shouldParseStm` expected

            it "fails if given a non-tape" $ do
                let state' = Env.fromList [("x", PVar SymType)]
                parseEvalState state' program "" `shouldFailOn` "write x 'x'"

        it "parses REJECT" $ do
            parseEmptyState program "" "reject" `shouldParseStm` Reject

        it "parses ACCEPT" $ do
            parseEmptyState program "" "accept" `shouldParseStm` Accept

    context "parsing composition" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        it "parses composition" $ do
            parseEvalState state program "" "left tape\n right tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))

        it "parses composition to be right associative" $ do
            let expected = Comp (MoveLeft (TapeVar ["tape"])) (Comp (MoveRight (TapeVar ["tape"])) (Write (TapeVar ["tape"]) (SymLit 'x')))
            parseEvalState state program "" "left tape \n right tape \n write tape 'x'" `shouldParseStm` expected

        it "allows for multiple newlines between statements" $ do
            parseEvalState state program "" "left tape \n\n right tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))
            parseEvalState state program "" "left tape \n\n\n right tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))

        it "fails if parsing the first statements fails to parse" $ do
            parseEvalState state program "" `shouldFailOn` "left tape \n if"

        it "fails if parsing the second statements fails to parse" $ do
            parseEvalState state program "" `shouldFailOn `"if \n left tape"

    context "parsing printing" $ do
        it "parses printing the a symbol" $ do
            let state = Env.fromList [("tape", PVar TapeType)]
            parseEvalState state program "" "print (read tape)" `shouldParseStm` (Print (Read (TapeVar ["tape"])))

        it "parses printing variables in structs" $ do
            let struct = ("S", PStruct [("x", SymType)])
                var    = ("s", PVar (CustomType "S"))
                state  = Env.fromList [struct, var]
            parseEvalState state program "" "print s.x" `shouldParseStm` (Print (SymVar ["s", "x"]))

    context "removing whitespace and comments" $ do
        let state = Env.fromList [("tape", PVar TapeType)]

        context "before statements" $ do
            it "ignores spaces" $ do
                parseEvalState state program "" " left tape" `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

            it "ignores newlines" $ do
                parseEvalState state program "" "\n\nleft tape" `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

            it "ignores whole-line comments" $ do
                parseEvalState state program "" "//Comment\n left tape" `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

            it "ignores in-line" $ do
                parseEvalState state program "" "/* Comment */\n left tape" `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

            it "ignores tabs" $ do
                parseEvalState state program "" "\tleft tape" `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

        context "interspersed with statements" $ do
            it "ignores whole line comments" $ do
                parseEvalState state program "" "left tape\n//Comment\n\n right tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))

            it "ignores in-line comments" $ do
                parseEvalState state program "" "if /* Comment */ True { left tape }" `shouldParseStm` (If TRUE (MoveLeft (TapeVar ["tape"])) [] Nothing)

        context "ignores whitespace after of statements" $ do
            it "ignores whitespace at the end of a statement" $ do
                parseEvalState state program "" "left tape  " `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

            it "ignores newlines at the end of a statement" $ do
                parseEvalState state program "" "left tape\n\n" `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

            it "ignores tabs" $ do
                parseEvalState state program "" "left tape\t" `shouldParseStm` (MoveLeft (TapeVar ["tape"]))

        context "composition" $ do
            it "ignores tabs after the newline" $ do
                parseEvalState state program "" "left tape\n\tright tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))

            it "ignores tabs before the newline" $ do
                parseEvalState state program "" "left tape\t\nright tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))

            it "ignores tabs alone on lines inbetween statements" $ do
                parseEvalState state program "" "left tape\n\t\nright tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))

            it "ignores spaces alone on lines inbetween statements" $ do
                parseEvalState state program "" "left tape\n \nright tape" `shouldParseStm` (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))

        context "whitespace nested in statements" $ do
            it "ignores newlines after an opening brace" $ do
                let expected = While TRUE (MoveLeft (TapeVar ["tape"]))
                parseEvalState state program "" "while True {\n\n left tape }" `shouldParseStm` expected

            it "ignores newlines before a closing brace" $ do
                let expected = While TRUE (MoveLeft (TapeVar ["tape"]))
                parseEvalState state program "" "while True { left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces" $ do
                let expected = While TRUE (MoveLeft (TapeVar ["tape"]))
                parseEvalState state program "" "while True { \n\n left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines after an opening brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))
                parseEvalState state program "" "while True {\n\n left tape \n\n right tape }" `shouldParseStm` expected

            it "ignores newlines after a closing brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))
                parseEvalState state program "" "while True { left tape \n\n right tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces when composing" $ do
                let expected = While TRUE (Comp (MoveLeft (TapeVar ["tape"])) (MoveRight (TapeVar ["tape"])))
                parseEvalState state program "" "while True { \n\n left tape \n\n right tape \n\n }" `shouldParseStm` expected
