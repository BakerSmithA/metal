module Syntax.ParserSpec (parserSpec) where

import Syntax.Tree
import Syntax.ParseState as S
import Syntax.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser
import Text.Megaparsec (parse)

parserSpec :: Spec
parserSpec = do
    describe "Parser" $ do
        encasedStringSpec
        tapeSymbolSpec
        varNameSpec
        funcNameSpec
        argNameSpec
        derivedSymbolSpec
        bexpSpec
        ifStmSpec
        programSpec
        importPathsSpec

encasedStringSpec :: Spec
encasedStringSpec = do
    describe "quotedString" $ do
        it "parses the empty string" $ do
            parseEmptyState quotedString "" "\"\"" `shouldParse` ""

        it "parses a string enclosed in double quotes" $ do
            parseEmptyState quotedString "" "\"str\"" `shouldParse` "str"

        it "parses strings enclosed in double quotes that contain spaces" $ do
            parseEmptyState quotedString "" "\"This is a string\"" `shouldParse` "This is a string"

        it "fails to parse if a starting quote is missing" $ do
            parseEmptyState quotedString "" `shouldFailOn` "str\""

        it "fails to parse if an ending quote is missing" $ do
            parseEmptyState quotedString "" `shouldFailOn` "\"str"

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

varNameSpec :: Spec
varNameSpec = do
    describe "varDeclName" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState varDeclName "" "vname" `shouldParse` "vname"

        it "parses names containing uppcase characters" $ do
            parseEmptyState varDeclName "" "vNAME" `shouldParse` "vNAME"

        it "parses names containing digits" $ do
            parseEmptyState varDeclName "" "v1234" `shouldParse` "v1234"

        it "parses if the variable name is a superset of a reserved word" $ do
            parseEmptyState varDeclName "" "trueV" `shouldParse` "trueV"

        it "fails if the start character is uppercase" $ do
            parseEmptyState varDeclName "" `shouldFailOn` "Vname"

        it "fails if the start character is a digit" $ do
            parseEmptyState varDeclName "" `shouldFailOn` "1vName"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState varDeclName "" `shouldFailOn` "True"

funcNameSpec :: Spec
funcNameSpec = do
    describe "funcDeclName" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState funcDeclName "" "fname" `shouldParse` "fname"

        it "parses names containing uppcase characters" $ do
            parseEmptyState funcDeclName "" "fNAME" `shouldParse` "fNAME"

        it "parses names containing digits" $ do
            parseEmptyState funcDeclName "" "f1234" `shouldParse` "f1234"

        it "parses if the function name is a superset of a reserved word" $ do
            parseEmptyState funcDeclName "" "trueF" `shouldParse` "trueF"

        it "fails if the start character is uppercase" $ do
            parseEmptyState funcDeclName "" `shouldFailOn` "Fname"

        it "fails if the start character is a digit" $ do
            parseEmptyState funcDeclName "" `shouldFailOn` "1fName"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState funcDeclName "" `shouldFailOn` "True"

argNameSpec :: Spec
argNameSpec = do
    describe "argName" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState argName "" "argname" `shouldParse` "argname"

        it "parses names containing uppcase characters" $ do
            parseEmptyState argName "" "argNAME" `shouldParse` "argNAME"

        it "parses names containing digits" $ do
            parseEmptyState argName "" "a1234" `shouldParse` "a1234"

        it "parses if the function name is a superset of a reserved word" $ do
            parseEmptyState argName "" "trueA" `shouldParse` "trueA"

        it "fails if the start character is uppercase" $ do
            parseEmptyState argName "" `shouldFailOn` "Aname"

        it "fails if the start character is a digit" $ do
            parseEmptyState argName "" `shouldFailOn` "1AName"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState argName "" `shouldFailOn` "True"

derivedSymbolSpec :: Spec
derivedSymbolSpec = do
    describe "derivedSymbol" $ do
        it "parses read" $ do
            parseEvalState (S.fromVarList ["tape"]) derivedSymbol "" "read tape" `shouldParse` (Read "tape")

        it "parses variable names" $ do
            parseEvalState (S.fromVarList ["x"]) derivedSymbol "" "x" `shouldParse` Var "x"

        it "parses literals" $ do
            parseEmptyState derivedSymbol "" "'b'" `shouldParse` Literal 'b'
            parseEmptyState derivedSymbol "" "'B'" `shouldParse` Literal 'B'
            parseEmptyState derivedSymbol "" "'1'" `shouldParse` Literal '1'
            parseEmptyState derivedSymbol "" "' '" `shouldParse` Literal ' '

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
            parseEvalState (S.fromVarList ["tape", "x"]) bexp "" "x == read tape" `shouldParse` (Eq (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            parseEvalState (S.fromVarList ["x", "y", "z"]) bexp "" "'x' == y == z" `shouldParse` (Eq (Literal 'x') (Var "y"))

    context "parsing LE operator" $ do
        it "parses LE" $ do
            parseEvalState (S.fromVarList ["x", "tape"]) bexp "" "x <= read tape" `shouldParse` (Le (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            parseEvalState (S.fromVarList ["x", "y", "z"]) bexp "" "x <= y <= z" `shouldParse` (Le (Var "x") (Var "y"))

    context "parsing NE operator" $ do
        it "parses NE" $ do
            parseEvalState (S.fromVarList ["x", "tape"]) bexp "" "x != read tape" `shouldParse` (Ne (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            parseEvalState (S.fromVarList ["x", "y", "z"]) bexp "" "x != y != z" `shouldParse` (Ne (Var "x") (Var "y"))

    context "parsing boolean expressions with parenthesis" $ do
        it "gives precedence to bracketed expressions" $ do
            parseEmptyState bexp "" "True and (False or False)" `shouldParse` (And TRUE (Or FALSE FALSE))

        it "allows brackets at the top level" $ do
            parseEmptyState bexp "" "(True)" `shouldParse` TRUE

ifStmSpec :: Spec
ifStmSpec = describe "ifStm" $ do
    context "parsing a single IF" $ do
        it "parses IF" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "if True { right tape }" `shouldParseStm` (If TRUE (MoveRight "tape") [] Nothing)

        it "fails to parse if a boolean expression is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True right tape"

    context "parsing an IF-ELSEIF" $ do
        it "parses with a single ELSE-IF clause" $ do
            let str      = "if True { right tape } else if False { left tape }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape"))] Nothing
            parseEvalState (S.fromVarList ["tape"]) program "" str `shouldParseStm` expected

        it "parses with multiple ELSE-IF clauses" $ do
            let str      = "if True { right tape } else if False { left tape } else if True { accept }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape")), (TRUE, Accept)] Nothing
            parseEvalState (S.fromVarList ["tape"]) program "" str `shouldParseStm` expected

        it "fails to parse if ELSE-IF is before IF" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "else if True { right tape } if True right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True { right tape } else if True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True { right tape } else if True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True { right tape } else if True right rape"

    context "parsing an ELSE clause" $ do
        it "parses ELSE with just an IF" $ do
            let str      = "if True { right tape } else { left tape }"
                expected = If TRUE (MoveRight "tape") [] (Just (MoveLeft "tape"))
            parseEvalState (S.fromVarList ["tape"]) program "" str `shouldParseStm` expected

        it "parses ELSE with a preceding ELSE-IF" $ do
            let str      = "if True { right tape } else if False { left tape } else { accept }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape"))] (Just Accept)
            parseEvalState (S.fromVarList ["tape"]) program "" str `shouldParseStm` expected

        it "fails to parse if the ELSE is before IF" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "else { accept } if { left tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True { right tape } else right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True { right tape } else { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "if True { right tape } else right tape"

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
        it "parses LEFT command" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "left tape" `shouldParseStm` (MoveLeft "tape")

        it "parses RIGHT command" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "right tape" `shouldParseStm` (MoveRight "tape")

        it "parses WRITE command" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "write tape 'x'" `shouldParseStm` (Write "tape" (Literal 'x'))

        it "parses a WRITESTR command" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "write tape \"abcd\"" `shouldParseStm` (WriteStr "tape" "abcd")

        it "parses REJECT" $ do
            parseEmptyState program "" "reject" `shouldParseStm` Reject

        it "parses ACCEPT" $ do
            parseEmptyState program "" "accept" `shouldParseStm` Accept

    context "parsing variable declarations" $ do
        it "parses variable declarations" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "let x = read tape" `shouldParseStm` VarDecl "x" (Read "tape")

        it "fails if '=' is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "let x read tape"

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

    context "parsing tape declarations" $ do
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

    context "parsing WHILE statements" $ do
        it "parses WHILE" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "while True { right tape }" `shouldParseStm` (While TRUE (MoveRight "tape"))

        it "fails to parse if a boolean expression is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "while { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "while True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "while True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "while True right tape"

    context "parsing function declarations" $ do
        it "parses function delcarations" $ do
            let expected = FuncDecl "fName" [] (MoveRight "tape")
            parseEvalState (S.fromVarList ["tape"]) program "" "func fName { right tape }" `shouldParseStm` expected

        it "parses function declarations with arguments" $ do
            let args = [FuncDeclArg "a" SymType, FuncDeclArg "bb" TapeType]
                expected = FuncDecl "fName" args (MoveRight "tape")
            parseEvalState (S.fromVarList ["tape"]) program "" "func fName a:Sym bb:Tape { right tape }" `shouldParseStm` expected

        it "parses function declarations where the name contains a keyword" $ do
            let expected = FuncDecl "leftUntil" [] (MoveRight "tape")
            parseEvalState (S.fromVarList ["tape"]) program "" "func leftUntil { right tape }" `shouldParseStm` expected

        it "fails to parse if a function name is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "func { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "func fName right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "func fName { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "func fName right tape"

        it "fails if the same function is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "func f { left main }\nfunc g { left main }"

    context "parsing function calls" $ do
        it "parses function calls" $ do
            parseEvalState (S.fromFuncList ["fName"]) program "" "fName" `shouldParseStm` (Call "fName" [])

        it "parses function calls with arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
                state    = S.fromLists ["tape", "x"] ["fName"]
            parseEvalState state program "" "fName (read tape) x '#'" `shouldParseStm` expected

        it "parses function calls with multiple spaces between arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
                state    = S.fromLists ["tape", "x"] ["fName"]
            parseEvalState state program "" "fName   (read tape)  x  '#'" `shouldParseStm` expected

        it "parses function calls with tabs between arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
                state    = S.fromLists ["tape", "x"] ["fName"]
            parseEvalState state program "" "fName \t(read tape)\tx\t'#'" `shouldParseStm` expected

        it "parses function calls followed by another statement" $ do
            let call     = Call "fName" [Derived (Read "tape")]
                expected = Comp call ((MoveLeft "tape"))
                state    = S.fromLists ["tape"] ["fName"]
            parseEvalState state program "" "fName (read tape) \n left tape" `shouldParseStm` expected

        it "parses function calls where the name contains a keyword" $ do
            let expected = Call "leftUntil" []
                state    = S.fromFuncList ["leftUntil"]
            parseEvalState state program "" "leftUntil" `shouldParseStm` expected

        it "parses tape literal arguments" $ do
            let expected = Call "f" [TapeLiteral "abcd", TapeLiteral "xyz"]
                state    = S.fromFuncList ["f"]
            parseEvalState state program "" "f \"abcd\" \"xyz\"" `shouldParseStm` expected

        it "fails if function has not been declared" $ do
            parseEmptyState program "" `shouldFailOn` "f"

    context "parsing composition" $ do
        it "parses composition" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "left tape\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        it "parses composition to be right associative" $ do
            let expected = Comp (MoveLeft "tape") (Comp (MoveRight "tape") (Write "tape" (Literal 'x')))
            parseEvalState (S.fromVarList ["tape"]) program "" "left tape \n right tape \n write tape 'x'" `shouldParseStm` expected

        it "allows for multiple newlines between statements" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "left tape \n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))
            parseEvalState (S.fromVarList ["tape"]) program "" "left tape \n\n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        it "fails if parsing the first statements fails to parse" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn` "left tape \n if"

        it "fails if parsing the second statements fails to parse" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" `shouldFailOn `"if \n left tape"

    context "parsing printing" $ do
        it "parses printing a string" $ do
            parseEmptyState program "" "print \"This is a string\"" `shouldParseStm` (PrintStr "This is a string")

        it "parses printing the symbol read from the tape" $ do
            parseEvalState (S.fromVarList ["tape"]) program "" "print tape" `shouldParseStm` (PrintRead "tape")

    context "removing whitespace and comments" $ do
        context "before statements" $ do
            it "ignores spaces" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" " left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores newlines" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "\n\nleft tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores whole-line comments" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "//Comment\n left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores in-line" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "/* Comment */\n left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores tabs" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "\tleft tape" `shouldParseStm` (MoveLeft "tape")

        context "interspersed with statements" $ do
            it "ignores whole line comments" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape\n//Comment\n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores in-line comments" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "if /* Comment */ True { left tape }" `shouldParseStm` (If TRUE (MoveLeft "tape") [] Nothing)

        context "ignores whitespace after of statements" $ do
            it "ignores whitespace at the end of a statement" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape  " `shouldParseStm` (MoveLeft "tape")

            it "ignores newlines at the end of a statement" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape\n\n" `shouldParseStm` (MoveLeft "tape")

            it "ignores tabs" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape\t" `shouldParseStm` (MoveLeft "tape")

        context "composition" $ do
            it "ignores tabs after the newline" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape\n\tright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores tabs before the newline" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape\t\nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores tabs alone on lines inbetween statements" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape\n\t\nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores spaces alone on lines inbetween statements" $ do
                parseEvalState (S.fromVarList ["tape"]) program "" "left tape\n \nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        context "whitespace nested in statements" $ do
            it "ignores newlines after an opening brace" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseEvalState (S.fromVarList ["tape"]) program "" "while True {\n\n left tape }" `shouldParseStm` expected

            it "ignores newlines before a closing brace" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseEvalState (S.fromVarList ["tape"]) program "" "while True { left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseEvalState (S.fromVarList ["tape"]) program "" "while True { \n\n left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines after an opening brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseEvalState (S.fromVarList ["tape"]) program "" "while True {\n\n left tape \n\n right tape }" `shouldParseStm` expected

            it "ignores newlines after a closing brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseEvalState (S.fromVarList ["tape"]) program "" "while True { left tape \n\n right tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseEvalState (S.fromVarList ["tape"]) program "" "while True { \n\n left tape \n\n right tape \n\n }" `shouldParseStm` expected
