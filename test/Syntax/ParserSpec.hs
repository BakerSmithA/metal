module Syntax.ParserSpec (parserSpec) where

import Syntax.Tree
import Syntax.Parser

import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

parserSpec :: Spec
parserSpec = do
    encasedStringSpec
    tapeSymbolSpec
    derivedSymbolSpec
    funcNameSpec
    bexpSpec
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

derivedSymbolSpec :: Spec
derivedSymbolSpec = do
    describe "derivedSymbol" $ do
        it "parses literals" $ do
            parse derivedSymbol "" "b" `shouldParse` Literal 'b'
            parse derivedSymbol "" "B" `shouldParse` Literal 'B'
            parse derivedSymbol "" "1" `shouldParse` Literal '1'

        it "parses read" $ do
            parse derivedSymbol "" "read" `shouldParse` Read

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
            parse bexp "" "x == read" `shouldParse` (Eq (Literal 'x') Read)

        it "fails to parse chains" $ do
            parse bexp "" "x == y == z" `shouldParse` (Eq (Literal 'x') (Literal 'y'))

    context "parsing LE operator" $ do
        it "parses LE" $ do
            parse bexp "" "x <= read" `shouldParse` (Le (Literal 'x') Read)

        it "fails to parse chains" $ do
            parse bexp "" "x <= y <= z" `shouldParse` (Le (Literal 'x') (Literal 'y'))

    context "parsing boolean expressions with parenthesis" $ do
        it "gives precedence to bracketed expressions" $ do
            parse bexp "" "True and (False or False)" `shouldParse` (And TRUE (Or FALSE FALSE))

        it "allows brackets at the top level" $ do
            parse bexp "" "(True)" `shouldParse` TRUE

stmSpec :: Spec
stmSpec = describe "stm" $ do
    context "parsing Turing Machine operators" $ do
        it "parses LEFT command" $ do
            parse stm "" "left" `shouldParse` MoveLeft

        it "parses RIGHT command" $ do
            parse stm "" "right" `shouldParse` MoveRight

        it "parses WRITE command" $ do
            parse stm "" "write x" `shouldParse` (Write 'x')

        it "parses REJECT" $ do
            parse stm "" "reject" `shouldParse` Reject

        it "parses ACCEPT" $ do
            parse stm "" "accept" `shouldParse` Accept

    context "parsing IF statements" $ do
        it "parses IF" $ do
            parse stm "" "if True { right }" `shouldParse` (If TRUE MoveRight)

        it "fails to parse if a boolean expression is missing" $ do
            parse stm "" `shouldFailOn` "if { right }"

        it "fails to parse if the first brace is missing" $ do
            parse stm "" `shouldFailOn` "if True right }"

        it "fails to parse if the second brace is missing" $ do
            parse stm "" `shouldFailOn` "if True { right"

        it "fails to parse if both braces are missing" $ do
            parse stm "" `shouldFailOn` "if True right"

    context "parsing IF-ELSE statements" $ do
        it "parses IF-ELSE" $ do
            parse stm "" "if True { right } else { left }" `shouldParse` (IfElse TRUE MoveRight MoveLeft)

        it "fails to parse if a boolean expression is missing" $ do
            parse stm "" `shouldFailOn` "if { right } else { left }"

        it "fails to parse if the first brace is missing" $ do
            parse stm "" `shouldFailOn` "if True right } else { left }"

        it "fails to parse if the second brace is missing" $ do
            parse stm "" `shouldFailOn` "if True { right else { left }"

        it "fails to parse if the third brace is missing" $ do
            parse stm "" `shouldFailOn` "if { right } else left }"

        it "fails to parse if the fourth brace is missing" $ do
            parse stm "" `shouldFailOn` "if { right } else { left"

        it "fails to parse if all braces are missing" $ do
            parse stm "" `shouldFailOn` "if right else left"

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
            parse stm "" "func fName { right }" `shouldParse` (Func "fName" MoveRight)

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
            parse stm "" "call fName" `shouldParse` (Call "fName")

        it "fails to parse if no function name is given" $ do
            parse stm "" `shouldFailOn` "call"

    context "parsing composition" $ do
        it "parses composition" $ do
            parse stm "" "left\n right" `shouldParse` (Comp MoveLeft MoveRight)

        it "allows for multiple newlines between statements" $ do
            parse stm "" "left \n\n right" `shouldParse` (Comp MoveLeft MoveRight)

    context "parsing printing" $ do
        it "parses printing a string" $ do
            parse stm "" "print \"This is a string\"" `shouldParse` (PrintStr "This is a string")

        it "parses printing the symbol read from the tape" $ do
            parse stm "" "print" `shouldParse` PrintRead

    context "removing whitespace and comments" $ do
        it "ignores whitespace at the start of a statement" $ do
            parse stm "" " left" `shouldParse` MoveLeft

        it "ignores whole-line comments at the start" $ do
            parse stm "" "//Comment\n left" `shouldParse` MoveLeft

        it "ignores in-line comments at the start" $ do
            parse stm "" "/* Comment */\n left" `shouldParse` MoveLeft

        it "ignores whole line comments" $ do
            parse stm "" "left\n//Comment\n\n right" `shouldParse` (Comp MoveLeft MoveRight)

        it "ignores in-line comments" $ do
            parse stm "" "if /* Comment */ True { left }" `shouldParse` (If TRUE MoveLeft)
