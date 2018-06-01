module Syntax.ParserSpec (parserSpec) where

import Syntax.Tree
import Syntax.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser

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
        importStmSpec

encasedStringSpec :: Spec
encasedStringSpec = do
    describe "quotedString" $ do
        it "parses the empty string" $ do
            parseM quotedString "" "\"\"" `shouldParse` ""

        it "parses a string enclosed in double quotes" $ do
            parseM quotedString "" "\"str\"" `shouldParse` "str"

        it "parses strings enclosed in double quotes that contain spaces" $ do
            parseM quotedString "" "\"This is a string\"" `shouldParse` "This is a string"

        it "fails to parse if a starting quote is missing" $ do
            parseM quotedString "" `shouldFailOn` "str\""

        it "fails to parse if an ending quote is missing" $ do
            parseM quotedString "" `shouldFailOn` "\"str"

tapeSymbolSpec :: Spec
tapeSymbolSpec = do
    describe "tapeSymbol" $ do
        it "parses lowercase letters" $ do
            parseM tapeSymbol "" "a" `shouldParse` 'a'

        it "parses uppercase letters" $ do
            parseM tapeSymbol "" "A" `shouldParse` 'A'

        it "parses digits" $ do
            parseM tapeSymbol "" "1" `shouldParse` '1'

        it "parses a ASCII symbols" $ do
            parseM tapeSymbol "" "+" `shouldParse` '+'
            parseM tapeSymbol "" "#" `shouldParse` '#'
            parseM tapeSymbol "" "<" `shouldParse` '<'
            parseM tapeSymbol "" "{" `shouldParse` '{'

        it "does not parse single quotes" $ do
            parseM tapeSymbol "" `shouldFailOn` "\'"

varNameSpec :: Spec
varNameSpec = do
    describe "varName" $ do
        it "parses names beginning with a lower char" $ do
            parseM funcName "" "vname" `shouldParse` "vname"

        it "parses names containing uppcase characters" $ do
            parseM funcName "" "vNAME" `shouldParse` "vNAME"

        it "parses names containing digits" $ do
            parseM funcName "" "v1234" `shouldParse` "v1234"

        it "parses if the variable name is a superset of a reserved word" $ do
            parseM funcName "" "trueV" `shouldParse` "trueV"

        it "fails if the start character is uppercase" $ do
            parseM funcName "" `shouldFailOn` "Vname"

        it "fails if the start character is a digit" $ do
            parseM funcName "" `shouldFailOn` "1vName"

        it "fails if the function name is a reserved keyword" $ do
            parseM funcName "" `shouldFailOn` "True"

funcNameSpec :: Spec
funcNameSpec = do
    describe "funcName" $ do
        it "parses names beginning with a lower char" $ do
            parseM funcName "" "fname" `shouldParse` "fname"

        it "parses names containing uppcase characters" $ do
            parseM funcName "" "fNAME" `shouldParse` "fNAME"

        it "parses names containing digits" $ do
            parseM funcName "" "f1234" `shouldParse` "f1234"

        it "parses if the function name is a superset of a reserved word" $ do
            parseM funcName "" "trueF" `shouldParse` "trueF"

        it "fails if the start character is uppercase" $ do
            parseM funcName "" `shouldFailOn` "Fname"

        it "fails if the start character is a digit" $ do
            parseM funcName "" `shouldFailOn` "1fName"

        it "fails if the function name is a reserved keyword" $ do
            parseM funcName "" `shouldFailOn` "True"

argNameSpec :: Spec
argNameSpec = do
    describe "argName" $ do
        it "parses names beginning with a lower char" $ do
            parseM funcName "" "argname" `shouldParse` "argname"

        it "parses names containing uppcase characters" $ do
            parseM funcName "" "argNAME" `shouldParse` "argNAME"

        it "parses names containing digits" $ do
            parseM funcName "" "a1234" `shouldParse` "a1234"

        it "parses if the function name is a superset of a reserved word" $ do
            parseM funcName "" "trueA" `shouldParse` "trueA"

        it "fails if the start character is uppercase" $ do
            parseM funcName "" `shouldFailOn` "Aname"

        it "fails if the start character is a digit" $ do
            parseM funcName "" `shouldFailOn` "1AName"

        it "fails if the function name is a reserved keyword" $ do
            parseM funcName "" `shouldFailOn` "True"

derivedSymbolSpec :: Spec
derivedSymbolSpec = do
    describe "derivedSymbol" $ do
        it "parses read" $ do
            parseM derivedSymbol "" "read tape" `shouldParse` (Read "tape")

        it "parses variable names" $ do
            parseM derivedSymbol "" "x" `shouldParse` Var "x"

        it "parses literals" $ do
            parseM derivedSymbol "" "'b'" `shouldParse` Literal 'b'
            parseM derivedSymbol "" "'B'" `shouldParse` Literal 'B'
            parseM derivedSymbol "" "'1'" `shouldParse` Literal '1'
            parseM derivedSymbol "" "' '" `shouldParse` Literal ' '

bexpSpec :: Spec
bexpSpec = describe "bexp" $ do
    context "parsing basis elements" $ do
        it "parses True" $ do
            parseM bexp "" "True" `shouldParse` TRUE

        it "parses False" $ do
            parseM bexp "" "False" `shouldParse` FALSE

    context "parsing NOT operator" $ do
        it "parses NOT" $ do
            parseM bexp "" "not False" `shouldParse` (Not FALSE)

    context "parsing AND operator" $ do
        it "parses AND" $ do
            parseM bexp "" "True and False" `shouldParse` (And TRUE FALSE)

        it "parse to be left associative" $ do
            parseM bexp "" "True and False and False" `shouldParse` (And (And TRUE FALSE) FALSE)

    context "parsing OR operator" $ do
        it "parses OR" $ do
            parseM bexp "" "True or False" `shouldParse` (Or TRUE FALSE)

        it "parses to be left associative" $ do
            parseM bexp "" "True or False or False" `shouldParse` (Or (Or TRUE FALSE) FALSE)

    context "parsing EQ operator" $ do
        it "parses EQ" $ do
            parseM bexp "" "x == read tape" `shouldParse` (Eq (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            parseM bexp "" "'x' == y == z" `shouldParse` (Eq (Literal 'x') (Var "y"))

    context "parsing LE operator" $ do
        it "parses LE" $ do
            parseM bexp "" "x <= read tape" `shouldParse` (Le (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            parseM bexp "" "x <= y <= z" `shouldParse` (Le (Var "x") (Var "y"))

    context "parsing NE operator" $ do
        it "parses NE" $ do
            parseM bexp "" "x != read tape" `shouldParse` (Ne (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            parseM bexp "" "x != y != z" `shouldParse` (Ne (Var "x") (Var "y"))

    context "parsing boolean expressions with parenthesis" $ do
        it "gives precedence to bracketed expressions" $ do
            parseM bexp "" "True and (False or False)" `shouldParse` (And TRUE (Or FALSE FALSE))

        it "allows brackets at the top level" $ do
            parseM bexp "" "(True)" `shouldParse` TRUE

ifStmSpec :: Spec
ifStmSpec = describe "ifStm" $ do
    context "parsing a single IF" $ do
        it "parses IF" $ do
            parseM program "" "if True { right tape }" `shouldParseStm` (If TRUE (MoveRight "tape") [] Nothing)

        it "fails to parse if a boolean expression is missing" $ do
            parseM program "" `shouldFailOn` "if { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseM program "" `shouldFailOn` "if True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseM program "" `shouldFailOn` "if True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseM program "" `shouldFailOn` "if True right tape"

    context "parsing an IF-ELSEIF" $ do
        it "parses with a single ELSE-IF clause" $ do
            let str      = "if True { right tape } else if False { left tape }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape"))] Nothing
            parseM program "" str `shouldParseStm` expected

        it "parses with multiple ELSE-IF clauses" $ do
            let str      = "if True { right tape } else if False { left tape } else if True { accept }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape")), (TRUE, Accept)] Nothing
            parseM program "" str `shouldParseStm` expected

        it "fails to parse if ELSE-IF is before IF" $ do
            parseM program "" `shouldFailOn` "else if True { right tape } if True right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseM program "" `shouldFailOn` "if True { right tape } else if True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseM program "" `shouldFailOn` "if True { right tape } else if True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseM program "" `shouldFailOn` "if True { right tape } else if True right rape"

    context "parsing an ELSE clause" $ do
        it "parses ELSE with just an IF" $ do
            let str      = "if True { right tape } else { left tape }"
                expected = If TRUE (MoveRight "tape") [] (Just (MoveLeft "tape"))
            parseM program "" str `shouldParseStm` expected

        it "parses ELSE with a preceding ELSE-IF" $ do
            let str      = "if True { right tape } else if False { left tape } else { accept }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape"))] (Just Accept)
            parseM program "" str `shouldParseStm` expected

        it "fails to parse if the ELSE is before IF" $ do
            parseM program "" `shouldFailOn` "else { accept } if { left tape }"

        it "fails to parse if the first brace is missing" $ do
            parseM program "" `shouldFailOn` "if True { right tape } else right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseM program "" `shouldFailOn` "if True { right tape } else { right tape"

        it "fails to parse if both braces are missing" $ do
            parseM program "" `shouldFailOn` "if True { right tape } else right tape"

importStmSpec :: Spec
importStmSpec = describe "importStm" $ do
    it "parses just a file name" $ do
        parseM importStm "" "import FileName" `shouldParse` "FileName"

    it "parses a file path" $ do
        parseM importStm "" "import Dir/SubDir/FileName" `shouldParse` "Dir/SubDir/FileName"

    it "parses when there is a backup dir" $ do
        parseM importStm "" "import ../Dir/../FileName" `shouldParse` "../Dir/../FileName"

programSpec :: Spec
programSpec = describe "program" $ do
    context "parsing Turing Machine operators" $ do
        it "parses LEFT command" $ do
            parseM program "" "left tape" `shouldParseStm` (MoveLeft "tape")

        it "parses RIGHT command" $ do
            parseM program "" "right tape" `shouldParseStm` (MoveRight "tape")

        it "parses WRITE command" $ do
            parseM program "" "write tape 'x'" `shouldParseStm` (Write "tape" (Literal 'x'))

        it "parses a WRITESTR command" $ do
            parseM program "" "write tape \"abcd\"" `shouldParseStm` (WriteStr "tape" "abcd")

        it "parses REJECT" $ do
            parseM program "" "reject" `shouldParseStm` Reject

        it "parses ACCEPT" $ do
            parseM program "" "accept" `shouldParseStm` Accept

    context "parsing variable declarations" $ do
        it "parses variable declarations" $ do
            parseM program "" "let x = read tape" `shouldParseStm` VarDecl "x" (Read "tape")

        it "fails if '=' is missing" $ do
            parseM program "" `shouldFailOn` "let x read tape"

        it "fails if a derived symbol is missing" $ do
            parseM program "" `shouldFailOn` "let x ="

        it "fails if the same variable is declared twice in the same scope" $ do
            parseM program "" `shouldFailOn` "let x = 'a'\nlet x = 'b'"

    context "parsing tape declarations" $ do
        it "parses tape declarations" $ do
            parseM program "" "let tape = \"abcd\"" `shouldParseStm` TapeDecl "tape" "abcd"

        it "fails if the same variable is declared twice in the same scope" $ do
            parseM program "" `shouldFailOn` "let tape = \"abc\"\nlet tape = \"xyz\""

    context "parsing WHILE statements" $ do
        it "parses WHILE" $ do
            parseM program "" "while True { right tape }" `shouldParseStm` (While TRUE (MoveRight "tape"))

        it "fails to parse if a boolean expression is missing" $ do
            parseM program "" `shouldFailOn` "while { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseM program "" `shouldFailOn` "while True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseM program "" `shouldFailOn` "while True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseM program "" `shouldFailOn` "while True right tape"

    context "parsing function declarations" $ do
        it "parses function delcarations" $ do
            let expected = FuncDecl "fName" [] (MoveRight "tape")
            parseM program "" "func fName { right tape }" `shouldParseStm` expected

        it "parses function declarations with arguments" $ do
            let args = [FuncDeclArg "a" SymType, FuncDeclArg "bb" TapeType]
                expected = FuncDecl "fName" args (MoveRight "tape")
            parseM program "" "func fName a:Sym bb:Tape { right tape }" `shouldParseStm` expected

        it "parses function declarations where the name contains a keyword" $ do
            let expected = FuncDecl "leftUntil" [] (MoveRight "tape")
            parseM program "" "func leftUntil { right tape }" `shouldParseStm` expected

        it "fails to parse if a function name is missing" $ do
            parseM program "" `shouldFailOn` "func { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseM program "" `shouldFailOn` "func fName right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseM program "" `shouldFailOn` "func fName { right tape"

        it "fails to parse if both braces are missing" $ do
            parseM program "" `shouldFailOn` "func fName right tape"

    context "parsing function calls" $ do
        it "parses function calls" $ do
            parseM program "" "fName" `shouldParseStm` (Call "fName" [])

        it "parses function calls with arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
            parseM program "" "fName (read tape) x '#'" `shouldParseStm` expected

        it "parses function calls with multiple spaces between arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
            parseM program "" "fName   (read tape)  x  '#'" `shouldParseStm` expected

        it "parses function calls with tabs between arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
            parseM program "" "fName \t(read tape)\tx\t'#'" `shouldParseStm` expected

        it "parses function calls followed by another statement" $ do
            let call = Call "fName" [Derived (Read "tape")]
                expected = Comp call ((MoveLeft "tape"))
            parseM program "" "fName (read tape) \n left tape" `shouldParseStm` expected

        it "parses function calls where the name contains a keyword" $ do
            let expected = Call "leftUntil" []
            parseM program "" "leftUntil" `shouldParseStm` expected

        it "parses tape literal arguments" $ do
            let expected = Call "f" [TapeLiteral "abcd", TapeLiteral "xyz"]
            parseM program "" "f \"abcd\" \"xyz\"" `shouldParseStm` expected

    context "parsing composition" $ do
        it "parses composition" $ do
            parseM program "" "left tape\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        it "parses composition to be right associative" $ do
            let expected = Comp (MoveLeft "tape") (Comp (MoveRight "tape") (Write "tape" (Literal 'x')))
            parseM program "" "left tape \n right tape \n write tape 'x'" `shouldParseStm` expected

        it "allows for multiple newlines between statements" $ do
            parseM program "" "left tape \n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))
            parseM program "" "left tape \n\n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        it "fails if parsing the first statements fails to parse" $ do
            parseM program "" `shouldFailOn` "left tape \n if"

        it "fails if parsing the second statements fails to parse" $ do
            parseM program "" `shouldFailOn `"if \n left tape"

    context "parsing printing" $ do
        it "parses printing a string" $ do
            parseM program "" "print \"This is a string\"" `shouldParseStm` (PrintStr "This is a string")

        it "parses printing the symbol read from the tape" $ do
            parseM program "" "print tape" `shouldParseStm` (PrintRead "tape")

    context "imports" $ do
        it "parses imports followed by a statement" $ do
            let expected = Program ["A/B", "C"] (MoveRight "tape")
            parseM program "" "import A/B\nimport C\nright tape" `shouldParse` expected

        it "fails if there is more than one newline between imports" $ do
            parseM program "" `shouldFailOn` "import A\n\nimport B\nleft"

    context "removing whitespace and comments" $ do
        context "before statements" $ do
            it "ignores spaces" $ do
                parseM program "" " left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores newlines" $ do
                parseM program "" "\n\nleft tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores whole-line comments" $ do
                parseM program "" "//Comment\n left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores in-line" $ do
                parseM program "" "/* Comment */\n left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores tabs" $ do
                parseM program "" "\tleft tape" `shouldParseStm` (MoveLeft "tape")

        context "before imports" $ do
            it "ignores spaces" $ do
                let expected = Program ["A"] (MoveLeft "tape")
                parseM program "" " import A\nleft tape" `shouldParse` expected

            it "ignores newlines" $ do
                let expected = Program ["A"] (MoveLeft "tape")
                parseM program "" "\n\nimport A\nleft tape" `shouldParse` expected

            it "ignores whole-line comments" $ do
                let expected = Program ["A"] (MoveLeft "tape")
                parseM program "" "//Comment\nimportA\nleft tape" `shouldParse` expected

            it "ignores in-line" $ do
                let expected = Program ["A"] (MoveLeft "tape")
                parseM program "" "/* Comment */\nimportA\nleft tape" `shouldParse` expected

        context "interspersed with statements" $ do
            it "ignores whole line comments" $ do
                parseM program "" "left tape\n//Comment\n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores in-line comments" $ do
                parseM program "" "if /* Comment */ True { left tape }" `shouldParseStm` (If TRUE (MoveLeft "tape") [] Nothing)

        context "ignores whitespace after of statements" $ do
            it "ignores whitespace at the end of a statement" $ do
                parseM program "" "left tape  " `shouldParseStm` (MoveLeft "tape")

            it "ignores newlines at the end of a statement" $ do
                parseM program "" "left tape\n\n" `shouldParseStm` (MoveLeft "tape")

            it "ignores tabs" $ do
                parseM program "" "left tape\t" `shouldParseStm` (MoveLeft "tape")

        context "composition" $ do
            it "ignores tabs after the newline" $ do
                parseM program "" "left tape\n\tright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores tabs before the newline" $ do
                parseM program "" "left tape\t\nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores tabs alone on lines inbetween statements" $ do
                parseM program "" "left tape\n\t\nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores spaces alone on lines inbetween statements" $ do
                parseM program "" "left tape\n \nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        context "whitespace nested in statements" $ do
            it "ignores newlines after an opening brace" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseM program "" "while True {\n\n left tape }" `shouldParseStm` expected

            it "ignores newlines before a closing brace" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseM program "" "while True { left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseM program "" "while True { \n\n left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines after an opening brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseM program "" "while True {\n\n left tape \n\n right tape }" `shouldParseStm` expected

            it "ignores newlines after a closing brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseM program "" "while True { left tape \n\n right tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseM program "" "while True { \n\n left tape \n\n right tape \n\n }" `shouldParseStm` expected
