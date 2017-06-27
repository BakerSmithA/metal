module Syntax.ParserSpec (parserSpec) where

import Syntax.Tree
import Syntax.Parser
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

parserSpec :: Spec
parserSpec = do
    describe "Parser" $ do
        encasedStringSpec
        tapeSymbolSpec
        varNameSpec
        funcNameSpec
        derivedSymbolSpec
        bexpSpec
        ifStmSpec
        stmSpec

encasedStringSpec :: Spec
encasedStringSpec = do
    describe "encasedString" $ do
        it "parses the empty string" $ do
            parse encasedString "" "\"\"" `shouldParse` ""

        it "parses a string enclosed in double quotes" $ do
            parse encasedString "" "\"str\"" `shouldParse` "str"

        it "parses strings enclosed in double quotes that contain spaces" $ do
            parse encasedString "" "\"This is a string\"" `shouldParse` "This is a string"

        it "fails to parse if a starting quote is missing" $ do
            parse encasedString "" `shouldFailOn` "str\""

        it "fails to parse if an ending quote is missing" $ do
            parse encasedString "" `shouldFailOn` "\"str"

tapeSymbolSpec :: Spec
tapeSymbolSpec = do
    describe "tapeSymbol" $ do
        it "parses lowercase letters" $ do
            parse tapeSymbol "" "a" `shouldParse` 'a'

        it "parses uppercase letters" $ do
            parse tapeSymbol "" "A" `shouldParse` 'A'

        it "parses digits" $ do
            parse tapeSymbol "" "1" `shouldParse` '1'

        it "parses a ASCII symbols" $ do
            parse tapeSymbol "" "+" `shouldParse` '+'
            parse tapeSymbol "" "#" `shouldParse` '#'
            parse tapeSymbol "" "<" `shouldParse` '<'
            parse tapeSymbol "" "{" `shouldParse` '{'

        it "does not parse single quotes" $ do
            parse tapeSymbol "" `shouldFailOn` "\'"

varNameSpec :: Spec
varNameSpec = do
    describe "varName" $ do
        it "parses names beginning with a lower char" $ do
            parse funcName "" "vname" `shouldParse` "vname"

        it "parses names containing uppcase characters" $ do
            parse funcName "" "vNAME" `shouldParse` "vNAME"

        it "parses names containing digits" $ do
            parse funcName "" "v1234" `shouldParse` "v1234"

        it "parses if the variable name is a superset of a reserved word" $ do
            parse funcName "" "trueV" `shouldParse` "trueV"

        it "fails if the start character is uppercase" $ do
            parse funcName "" `shouldFailOn` "Vname"

        it "fails if the start character is a digit" $ do
            parse funcName "" `shouldFailOn` "1vName"

        it "fails if the function name is a reserved keyword" $ do
            parse funcName "" `shouldFailOn` "True"

funcNameSpec :: Spec
funcNameSpec = do
    describe "funcName" $ do
        it "parses names beginning with a lower char" $ do
            parse funcName "" "fname" `shouldParse` "fname"

        it "parses names containing uppcase characters" $ do
            parse funcName "" "fNAME" `shouldParse` "fNAME"

        it "parses names containing digits" $ do
            parse funcName "" "f1234" `shouldParse` "f1234"

        it "parses if the function name is a superset of a reserved word" $ do
            parse funcName "" "trueF" `shouldParse` "trueF"

        it "fails if the start character is uppercase" $ do
            parse funcName "" `shouldFailOn` "Fname"

        it "fails if the start character is a digit" $ do
            parse funcName "" `shouldFailOn` "1fName"

        it "fails if the function name is a reserved keyword" $ do
            parse funcName "" `shouldFailOn` "True"

derivedSymbolSpec :: Spec
derivedSymbolSpec = do
    describe "derivedSymbol" $ do
        it "parses read" $ do
            parse derivedSymbol "" "read" `shouldParse` Read

        it "parses variable names" $ do
            parse derivedSymbol "" "x" `shouldParse` Var "x"

        it "parses literals" $ do
            parse derivedSymbol "" "'b'" `shouldParse` Literal 'b'
            parse derivedSymbol "" "'B'" `shouldParse` Literal 'B'
            parse derivedSymbol "" "'1'" `shouldParse` Literal '1'
            parse derivedSymbol "" "' '" `shouldParse` Literal ' '

bexpSpec :: Spec
bexpSpec = describe "bexp" $ do
    context "parsing basis elements" $ do
        it "parses True" $ do
            parse bexp "" "True" `shouldParse` TRUE

        it "parses False" $ do
            parse bexp "" "False" `shouldParse` FALSE

    context "parsing NOT operator" $ do
        it "parses NOT" $ do
            parse bexp "" "not False" `shouldParse` (Not FALSE)

    context "parsing AND operator" $ do
        it "parses AND" $ do
            parse bexp "" "True and False" `shouldParse` (And TRUE FALSE)

        it "parse to be left associative" $ do
            parse bexp "" "True and False and False" `shouldParse` (And (And TRUE FALSE) FALSE)

    context "parsing OR operator" $ do
        it "parses OR" $ do
            parse bexp "" "True or False" `shouldParse` (Or TRUE FALSE)

        it "parses to be left associative" $ do
            parse bexp "" "True or False or False" `shouldParse` (Or (Or TRUE FALSE) FALSE)

    context "parsing EQ operator" $ do
        it "parses EQ" $ do
            parse bexp "" "x == read" `shouldParse` (Eq (Var "x") Read)

        it "fails to parse chains" $ do
            parse bexp "" "'x' == y == z" `shouldParse` (Eq (Literal 'x') (Var "y"))

    context "parsing LE operator" $ do
        it "parses LE" $ do
            parse bexp "" "x <= read" `shouldParse` (Le (Var "x") Read)

        it "fails to parse chains" $ do
            parse bexp "" "x <= y <= z" `shouldParse` (Le (Var "x") (Var "y"))

    context "parsing boolean expressions with parenthesis" $ do
        it "gives precedence to bracketed expressions" $ do
            parse bexp "" "True and (False or False)" `shouldParse` (And TRUE (Or FALSE FALSE))

        it "allows brackets at the top level" $ do
            parse bexp "" "(True)" `shouldParse` TRUE

ifStmSpec :: Spec
ifStmSpec = describe "ifStm" $ do
    context "parsing a single IF" $ do
        it "parses IF" $ do
            parse stm "" "if True { right }" `shouldParse` (If TRUE MoveRight [] Nothing)

        it "fails to parse if a boolean expression is missing" $ do
            parse stm "" `shouldFailOn` "if { right }"

        it "fails to parse if the first brace is missing" $ do
            parse stm "" `shouldFailOn` "if True right }"

        it "fails to parse if the second brace is missing" $ do
            parse stm "" `shouldFailOn` "if True { right"

        it "fails to parse if both braces are missing" $ do
            parse stm "" `shouldFailOn` "if True right"

    context "parsing an IF-ELSEIF" $ do
        it "parses with a single ELSE-IF clause" $ do
            let str      = "if True { right } else if False { left }"
                expected = If TRUE MoveRight [(FALSE, MoveLeft)] Nothing
            parse stm "" str `shouldParse` expected

        it "parses with multiple ELSE-IF clauses" $ do
            let str      = "if True { right } else if False { left } else if True { accept }"
                expected = If TRUE MoveRight [(FALSE, MoveLeft), (TRUE, Accept)] Nothing
            parse stm "" str `shouldParse` expected

        it "fails to parse if ELSE-IF is before IF" $ do
            parse stm "" `shouldFailOn` "else if True { right } if True right }"

        it "fails to parse if the first brace is missing" $ do
            parse stm "" `shouldFailOn` "if True { right } else if True right }"

        it "fails to parse if the second brace is missing" $ do
            parse stm "" `shouldFailOn` "if True { right } else if True { right"

        it "fails to parse if both braces are missing" $ do
            parse stm "" `shouldFailOn` "if True { right } else if True right"

    context "parsing an ELSE clause" $ do
        it "parses ELSE with just an IF" $ do
            let str      = "if True { right } else { left }"
                expected = If TRUE MoveRight [] (Just MoveLeft)
            parse stm "" str `shouldParse` expected

        it "parses ELSE with a preceding ELSE-IF" $ do
            let str      = "if True { right } else if False { left } else { accept }"
                expected = If TRUE MoveRight [(FALSE, MoveLeft)] (Just Accept)
            parse stm "" str `shouldParse` expected

        it "fails to parse if the ELSE is before IF" $ do
            parse stm "" `shouldFailOn` "else { accept } if { left }"

        it "fails to parse if the first brace is missing" $ do
            parse stm "" `shouldFailOn` "if True { right } else right }"

        it "fails to parse if the second brace is missing" $ do
            parse stm "" `shouldFailOn` "if True { right } else { right"

        it "fails to parse if both braces are missing" $ do
            parse stm "" `shouldFailOn` "if True { right } else right"

stmSpec :: Spec
stmSpec = describe "stm" $ do
    context "parsing Turing Machine operators" $ do
        it "parses LEFT command" $ do
            parse stm "" "left" `shouldParse` MoveLeft

        it "parses RIGHT command" $ do
            parse stm "" "right" `shouldParse` MoveRight

        it "parses WRITE command" $ do
            parse stm "" "write 'x'" `shouldParse` (Write (Literal 'x'))

        it "parses REJECT" $ do
            parse stm "" "reject" `shouldParse` Reject

        it "parses ACCEPT" $ do
            parse stm "" "accept" `shouldParse` Accept

    context "parsing variable declarations" $ do
        it "parses variable declarations" $ do
            parse stm "" "let x = read" `shouldParse` VarDecl "x" Read

        it "fails if '=' is missing" $ do
            parse stm "" `shouldFailOn` "let x read"

        it "fails if a derived symbol is missing" $ do
            parse stm "" `shouldFailOn` "let x ="

    context "parsing WHILE statements" $ do
        it "parses WHILE" $ do
            parse stm "" "while True { right }" `shouldParse` (While TRUE MoveRight)

        it "fails to parse if a boolean expression is missing" $ do
            parse stm "" `shouldFailOn` "while { right }"

        it "fails to parse if the first brace is missing" $ do
            parse stm "" `shouldFailOn` "while True right }"

        it "fails to parse if the second brace is missing" $ do
            parse stm "" `shouldFailOn` "while True { right"

        it "fails to parse if both braces are missing" $ do
            parse stm "" `shouldFailOn` "while True right"

    context "parsing function declarations" $ do
        it "parses function delcarations" $ do
            let expected = FuncDecl "fName" [] MoveRight
            parse stm "" "func fName { right }" `shouldParse` expected

        it "parses function declarations with arguments" $ do
            let expected = FuncDecl "fName" ["a", "bb", "ccc"] MoveRight
            parse stm "" "func fName a bb ccc { right }" `shouldParse` expected

        it "fails to parse if a function name is missing" $ do
            parse stm "" `shouldFailOn` "func { right }"

        it "fails to parse if the first brace is missing" $ do
            parse stm "" `shouldFailOn` "func fName right }"

        it "fails to parse if the second brace is missing" $ do
            parse stm "" `shouldFailOn` "func fName { right"

        it "fails to parse if both braces are missing" $ do
            parse stm "" `shouldFailOn` "func fName right"

    context "parsing function calls" $ do
        it "parses function calls" $ do
            parse stm "" "fName" `shouldParse` (Call "fName" [])

        it "parses function calls with arguments" $ do
            let expected = Call "fName" [Read, Var "x", Literal '#']
            parse stm "" "fName read x '#'" `shouldParse` expected

    context "parsing composition" $ do
        it "parses composition" $ do
            parse stm "" "left\n right" `shouldParse` (Comp MoveLeft MoveRight)

        it "parses composition to be right associative" $ do
            let expected = Comp MoveLeft (Comp MoveRight (Write (Literal 'x')))
            parse stm "" "left \n right \n write 'x'" `shouldParse` expected

        it "allows for multiple newlines between statements" $ do
            parse stm "" "left \n\n right" `shouldParse` (Comp MoveLeft MoveRight)
            parse stm "" "left \n\n\n right" `shouldParse` (Comp MoveLeft MoveRight)

        it "fails if parsing the first statements fails to parse" $ do
            parse stm "" `shouldFailOn` "left \n if"

        it "fails if parsing the second statements fails to parse" $ do
            parse stm "" `shouldFailOn `"if \n left"

    context "parsing printing" $ do
        it "parses printing a string" $ do
            parse stm "" "print \"This is a string\"" `shouldParse` (PrintStr "This is a string")

        it "parses printing the symbol read from the tape" $ do
            parse stm "" "print" `shouldParse` PrintRead

    context "removing whitespace and comments" $ do
        context "before statements" $ do
            it "ignores spaces" $ do
                parse stm "" " left" `shouldParse` MoveLeft

            it "ignores newlines" $ do
                parse stm "" "\n\nleft" `shouldParse` MoveLeft

            it "ignores whole-line comments" $ do
                parse stm "" "//Comment\n left" `shouldParse` MoveLeft

            it "ignores in-line" $ do
                parse stm "" "/* Comment */\n left" `shouldParse` MoveLeft

            it "ignores tabs" $ do
                parse stm "" "\tleft" `shouldParse` MoveLeft

        context "interspersed with statements" $ do
            it "ignores whole line comments" $ do
                parse stm "" "left\n//Comment\n\n right" `shouldParse` (Comp MoveLeft MoveRight)

            it "ignores in-line comments" $ do
                parse stm "" "if /* Comment */ True { left }" `shouldParse` (If TRUE MoveLeft [] Nothing)

        context "ignores whitespace after of statements" $ do
            it "ignores whitespace at the end of a statement" $ do
                parse stm "" "left   " `shouldParse` MoveLeft

            it "ignores newlines at the end of a statement" $ do
                parse stm "" "left\n\n " `shouldParse` MoveLeft

            it "ignores tabs" $ do
                parse stm "" "left\t" `shouldParse` MoveLeft

        context "composition" $ do
            it "ignores tabs after the newline" $ do
                parse stm "" "left\n\tright" `shouldParse` (Comp MoveLeft MoveRight)

            it "ignores tabs before the newline" $ do
                parse stm "" "left\t\nright" `shouldParse` (Comp MoveLeft MoveRight)

            it "ignores tabs alone on lines inbetween statements" $ do
                parse stm "" "left\n\t\nright" `shouldParse` (Comp MoveLeft MoveRight)

            it "ignores spaces alone on lines inbetween statements" $ do
                parse stm "" "left\n \nright" `shouldParse` (Comp MoveLeft MoveRight)

        context "whitespace nested in statements" $ do
            it "ignores newlines after an opening brace" $ do
                parse stm "" "while True {\n\n left }" `shouldParse` (While TRUE MoveLeft)

            it "ignores newlines before a closing brace" $ do
                parse stm "" "while True { left \n\n }" `shouldParse` (While TRUE MoveLeft)

            it "ignores newlines before and after braces" $ do
                parse stm "" "while True { \n\n left \n\n }" `shouldParse` (While TRUE MoveLeft)

            it "ignores newlines after an opening brace when composing" $ do
                parse stm "" "while True {\n\n left \n\n right }" `shouldParse` (While TRUE (Comp MoveLeft MoveRight))

            it "ignores newlines after a closing brace when composing" $ do
                parse stm "" "while True { left \n\n right \n\n }" `shouldParse` (While TRUE (Comp MoveLeft MoveRight))

            it "ignores newlines before and after braces when composing" $ do
                parse stm "" "while True { \n\n left \n\n right \n\n }" `shouldParse` (While TRUE (Comp MoveLeft MoveRight))
