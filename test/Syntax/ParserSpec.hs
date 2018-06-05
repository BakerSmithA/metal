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
        newArgSpec
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
    describe "newVar" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState (newVar SymType) "" "vname" `shouldParse` "vname"

        it "parses names containing uppcase characters" $ do
            parseEmptyState (newVar SymType) "" "vNAME" `shouldParse` "vNAME"

        it "parses names containing digits" $ do
            parseEmptyState (newVar SymType) "" "v1234" `shouldParse` "v1234"

        it "parses if the variable name is a superset of a reserved word" $ do
            parseEmptyState (newVar SymType) "" "trueV" `shouldParse` "trueV"

        it "fails if the start character is uppercase" $ do
            parseEmptyState (newVar SymType) "" `shouldFailOn` "Vname"

        it "fails if the start character is a digit" $ do
            parseEmptyState (newVar SymType) "" `shouldFailOn` "1vName"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState (newVar SymType) "" `shouldFailOn` "True"

funcNameSpec :: Spec
funcNameSpec = do
    describe "newFunc" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState newFunc "" "fname" `shouldParse` "fname"

        it "parses names containing uppcase characters" $ do
            parseEmptyState newFunc "" "fNAME" `shouldParse` "fNAME"

        it "parses names containing digits" $ do
            parseEmptyState newFunc "" "f1234" `shouldParse` "f1234"

        it "parses if the function name is a superset of a reserved word" $ do
            parseEmptyState newFunc "" "trueF" `shouldParse` "trueF"

        it "fails if the start character is uppercase" $ do
            parseEmptyState newFunc "" `shouldFailOn` "Fname"

        it "fails if the start character is a digit" $ do
            parseEmptyState newFunc "" `shouldFailOn` "1fName"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState newFunc "" `shouldFailOn` "True"

newArgSpec :: Spec
newArgSpec = do
    describe "newArg" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState newArg "" "argname" `shouldParse` "argname"

        it "parses names containing uppcase characters" $ do
            parseEmptyState newArg "" "argNAME" `shouldParse` "argNAME"

        it "parses names containing digits" $ do
            parseEmptyState newArg "" "a1234" `shouldParse` "a1234"

        it "parses if the function name is a superset of a reserved word" $ do
            parseEmptyState newArg "" "trueA" `shouldParse` "trueA"

        it "fails if the start character is uppercase" $ do
            parseEmptyState newArg "" `shouldFailOn` "Aname"

        it "fails if the start character is a digit" $ do
            parseEmptyState newArg "" `shouldFailOn` "1AName"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState newArg "" `shouldFailOn` "True"

derivedSymbolSpec :: Spec
derivedSymbolSpec = do
    describe "derivedSymbol" $ do
        it "parses read" $ do
            let state = S.fromVarList [("tape", TapeType)]
            parseEvalState state (derivedSymbol SymType) "" "read tape" `shouldParse` (Read "tape")

        it "parses symbol variables" $ do
            let state = S.fromVarList [("x", SymType)]
            parseEvalState state (derivedSymbol SymType) "" "x" `shouldParse` Var "x"

        it "parses tape variables" $ do
            let state = S.fromVarList [("x", TapeType)]
            parseEvalState state (derivedSymbol TapeType) "" "x" `shouldParse` Var "x"

        it "fails to parse variables if the types mismatch" $ do
            let state = S.fromVarList [("x", SymType)]
            parseEvalState state (derivedSymbol TapeType) "" `shouldFailOn` "x"

        it "parses literals" $ do
            parseEmptyState (derivedSymbol SymType) "" "'b'" `shouldParse` Literal 'b'
            parseEmptyState (derivedSymbol SymType) "" "'B'" `shouldParse` Literal 'B'
            parseEmptyState (derivedSymbol SymType) "" "'1'" `shouldParse` Literal '1'
            parseEmptyState (derivedSymbol SymType) "" "' '" `shouldParse` Literal ' '

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
            let state = S.fromVarList [("tape", TapeType), ("x", SymType)]
            parseEvalState state bexp "" "x == read tape" `shouldParse` (Eq (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            let state = S.fromVarList [("x", SymType), ("y", SymType), ("z", SymType)]
            parseEvalState state bexp "" "'x' == y == z" `shouldParse` (Eq (Literal 'x') (Var "y"))

    context "parsing LE operator" $ do
        it "parses LE" $ do
            let state = S.fromVarList [("x", SymType), ("tape", TapeType)]
            parseEvalState state bexp "" "x <= read tape" `shouldParse` (Le (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            let state = S.fromVarList [("x", SymType), ("y", SymType), ("z", SymType)]
            parseEvalState state bexp "" "x <= y <= z" `shouldParse` (Le (Var "x") (Var "y"))

    context "parsing NE operator" $ do
        it "parses NE" $ do
            let state = S.fromVarList [("x", SymType), ("tape", TapeType)]
            parseEvalState state bexp "" "x != read tape" `shouldParse` (Ne (Var "x") (Read "tape"))

        it "fails to parse chains" $ do
            let state = S.fromVarList [("x", SymType), ("y", SymType), ("z", SymType)]
            parseEvalState state bexp "" "x != y != z" `shouldParse` (Ne (Var "x") (Var "y"))

    context "parsing boolean expressions with parenthesis" $ do
        it "gives precedence to bracketed expressions" $ do
            parseEmptyState bexp "" "True and (False or False)" `shouldParse` (And TRUE (Or FALSE FALSE))

        it "allows brackets at the top level" $ do
            parseEmptyState bexp "" "(True)" `shouldParse` TRUE

ifStmSpec :: Spec
ifStmSpec = describe "ifStm" $ do
    let state = S.fromVarList [("tape", TapeType)]

    context "parsing a single IF" $ do
        it "parses IF" $ do
            parseEvalState state program "" "if True { right tape }" `shouldParseStm` (If TRUE (MoveRight "tape") [] Nothing)

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
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape"))] Nothing
            parseEvalState state program "" str `shouldParseStm` expected

        it "parses with multiple ELSE-IF clauses" $ do
            let str      = "if True { right tape } else if False { left tape } else if True { accept }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape")), (TRUE, Accept)] Nothing
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
                expected = If TRUE (MoveRight "tape") [] (Just (MoveLeft "tape"))
            parseEvalState state program "" str `shouldParseStm` expected

        it "parses ELSE with a preceding ELSE-IF" $ do
            let str      = "if True { right tape } else if False { left tape } else { accept }"
                expected = If TRUE (MoveRight "tape") [(FALSE, (MoveLeft "tape"))] (Just Accept)
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
            let innerVarDecl = VarDecl "x" (Literal 'a')
                ifStm        = If TRUE innerVarDecl [] Nothing
                outerVarDecl = TapeDecl "x" "xyz"
                comp         = Comp outerVarDecl ifStm
            parseEvalState state program "" "let x = \"xyz\" \n if True { let x = 'a' }" `shouldParseStm` comp

        it "allows the types of variables to be changed at inner scopes" $ do
            let innerVarDecl = VarDecl "x" (Literal 'a')
                write        = Write "tape" (Var "x")
                body         = Comp innerVarDecl write
                ifStm        = If TRUE body [] Nothing
                outerVarDecl = TapeDecl "x" "xyz"
                comp         = Comp outerVarDecl ifStm
            parseEvalState state program "" "let x = \"xyz\" \n if True { let x = 'a' \n write tape x }" `shouldParseStm` comp

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
        let state = S.fromVarList [("tape", TapeType)]

        it "parses LEFT command" $ do
            parseEvalState state program "" "left tape" `shouldParseStm` (MoveLeft "tape")

        it "parses RIGHT command" $ do
            parseEvalState state program "" "right tape" `shouldParseStm` (MoveRight "tape")

        it "parses WRITE command" $ do
            parseEvalState state program "" "write tape 'x'" `shouldParseStm` (Write "tape" (Literal 'x'))

        it "parses a WRITESTR command" $ do
            parseEvalState state program "" "write tape \"abcd\"" `shouldParseStm` (WriteStr "tape" "abcd")

        it "parses REJECT" $ do
            parseEmptyState program "" "reject" `shouldParseStm` Reject

        it "parses ACCEPT" $ do
            parseEmptyState program "" "accept" `shouldParseStm` Accept

    context "parsing variable declarations" $ do
        let state = S.fromVarList [("tape", TapeType)]

        it "parses variable declarations" $ do
            parseEvalState state program "" "let x = read tape" `shouldParseStm` VarDecl "x" (Read "tape")

        it "fails if '=' is missing" $ do
            parseEvalState state program "" `shouldFailOn` "let x read tape"

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
        let state = S.fromVarList [("tape", TapeType)]

        it "parses WHILE" $ do
            parseEvalState state program "" "while True { right tape }" `shouldParseStm` (While TRUE (MoveRight "tape"))

        it "fails to parse if a boolean expression is missing" $ do
            parseEvalState state program "" `shouldFailOn` "while { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "while True right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "while True { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState state program "" `shouldFailOn` "while True right tape"

    context "parsing function declarations" $ do
        let state = S.fromVarList [("tape", TapeType)]

        it "parses function delcarations" $ do
            let expected = FuncDecl "fName" [] (MoveRight "tape")
            parseEvalState state program "" "func fName { right tape }" `shouldParseStm` expected

        it "parses function declarations with arguments" $ do
            let args = [FuncDeclArg "a" SymType, FuncDeclArg "bb" TapeType]
                expected = FuncDecl "fName" args (MoveRight "tape")
            parseEvalState state program "" "func fName a:Sym bb:Tape { right tape }" `shouldParseStm` expected

        it "parses function declarations where the name contains a keyword" $ do
            let expected = FuncDecl "leftUntil" [] (MoveRight "tape")
            parseEvalState state program "" "func leftUntil { right tape }" `shouldParseStm` expected

        it "allows variables to be shadowed" $ do
            let innerVarDecl = VarDecl "x" (Literal 'a')
                func         = FuncDecl "f" [] innerVarDecl
                outerVarDecl = TapeDecl "x" "xyz"
                comp         = Comp outerVarDecl func
            parseEvalState state program "" "let x = \"xyz\" \n func f { let x = 'a' }" `shouldParseStm` comp

        it "allows the types of variables to be changed at inner scopes" $ do
            let innerVarDecl = VarDecl "x" (Literal 'a')
                write        = Write "tape" (Var "x")
                body         = Comp innerVarDecl write
                func         = FuncDecl "f" [] body
                outerVarDecl = TapeDecl "x" "xyz"
                comp         = Comp outerVarDecl func
            parseEvalState state program "" "let x = \"xyz\" \n func f { let x = 'a' \n write tape x }" `shouldParseStm` comp

        it "allows arguments to be used inside the function" $ do
            let args = [FuncDeclArg "t" TapeType, FuncDeclArg "x" SymType]
                func = FuncDecl "writeNew" args (Write "t" (Var "x"))
            parseEmptyState program "" "func writeNew t:Tape x:Sym { write t x }" `shouldParseStm` func

        it "fails if argument names are duplicated" $ do
            parseEmptyState program "" `shouldFailOn` "func f x:Tape x:Sym { print \"\" }"

        it "fails to parse if a function name is missing" $ do
            parseEvalState state program "" `shouldFailOn` "func { right tape }"

        it "fails to parse if the first brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "func fName right tape }"

        it "fails to parse if the second brace is missing" $ do
            parseEvalState state program "" `shouldFailOn` "func fName { right tape"

        it "fails to parse if both braces are missing" $ do
            parseEvalState state program "" `shouldFailOn` "func fName right tape"

        it "fails if the same function is declared twice in the same scope" $ do
            parseEmptyState program "" `shouldFailOn` "func f { left main }\nfunc g { left main }"

    context "parsing function calls" $ do
        it "parses function calls" $ do
            let state = S.fromFuncList [("fName", [])]
            parseEvalState state program "" "fName" `shouldParseStm` (Call "fName" [])

        it "parses function calls with arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
                var1     = ("tape", TapeType)
                var2     = ("x", SymType)
                func     = ("fName", [SymType, SymType, SymType])
                state    = S.fromLists [var1, var2] [func]
            parseEvalState state program "" "fName (read tape) x '#'" `shouldParseStm` expected

        it "parses function calls with multiple spaces between arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
                var1     = ("tape", TapeType)
                var2     = ("x", SymType)
                func     = ("fName", [SymType, SymType, SymType])
                state    = S.fromLists [var1, var2] [func]
            parseEvalState state program "" "fName   (read tape)  x  '#'" `shouldParseStm` expected

        it "parses function calls with tabs between arguments" $ do
            let expected = Call "fName" [Derived (Read "tape"), Derived (Var "x"), Derived (Literal '#')]
                var1     = ("tape", TapeType)
                var2     = ("x", SymType)
                func     = ("fName", [SymType, SymType, SymType])
                state    = S.fromLists [var1, var2] [func]
            parseEvalState state program "" "fName \t(read tape)\tx\t'#'" `shouldParseStm` expected

        it "parses function calls followed by another statement" $ do
            let call     = Call "fName" [Derived (Read "tape")]
                expected = Comp call ((MoveLeft "tape"))
                var      = ("tape", TapeType)
                func     = ("fName", [SymType])
                state    = S.fromLists [var] [func]
            parseEvalState state program "" "fName (read tape) \n left tape" `shouldParseStm` expected

        it "parses function calls where the name contains a keyword" $ do
            let expected = Call "leftUntil" []
                state    = S.fromFuncList [("leftUntil", [])]
            parseEvalState state program "" "leftUntil" `shouldParseStm` expected

        it "parses tape literal arguments" $ do
            let expected = Call "f" [TapeLiteral "abcd", TapeLiteral "xyz"]
                state    = S.fromFuncList [("f", [TapeType, TapeType])]
            parseEvalState state program "" "f \"abcd\" \"xyz\"" `shouldParseStm` expected

        it "fails if function has not been declared" $ do
            parseEmptyState program "" `shouldFailOn` "f"

        it "fails if the argument types mismatch" $ do
            let state = S.fromLists [("x", TapeType), ("y", SymType)] [("f", [TapeType, SymType])]
            parseEvalState state program "" `shouldFailOn` "f y x"

        it "fails if an incorrect number of arguments are supplied" $ do
            let state = S.fromLists [("x", TapeType), ("y", SymType)] [("f", [TapeType, SymType])]
            parseEvalState state program "" `shouldFailOn` "f"
            parseEvalState state program "" `shouldFailOn` "f x"
            parseEvalState state program "" `shouldFailOn` "f x y y"

    context "parsing composition" $ do
        let state = S.fromVarList [("tape", TapeType)]

        it "parses composition" $ do
            parseEvalState state program "" "left tape\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        it "parses composition to be right associative" $ do
            let expected = Comp (MoveLeft "tape") (Comp (MoveRight "tape") (Write "tape" (Literal 'x')))
            parseEvalState state program "" "left tape \n right tape \n write tape 'x'" `shouldParseStm` expected

        it "allows for multiple newlines between statements" $ do
            parseEvalState state program "" "left tape \n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))
            parseEvalState state program "" "left tape \n\n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        it "fails if parsing the first statements fails to parse" $ do
            parseEvalState state program "" `shouldFailOn` "left tape \n if"

        it "fails if parsing the second statements fails to parse" $ do
            parseEvalState state program "" `shouldFailOn `"if \n left tape"

    context "parsing printing" $ do
        it "parses printing a string" $ do
            parseEmptyState program "" "print \"This is a string\"" `shouldParseStm` (PrintStr "This is a string")

        it "parses printing the symbol read from the tape" $ do
            let state = S.fromVarList [("tape", TapeType)]
            parseEvalState state program "" "print tape" `shouldParseStm` (PrintRead "tape")

    context "removing whitespace and comments" $ do
        let state = S.fromVarList [("tape", TapeType)]

        context "before statements" $ do
            it "ignores spaces" $ do
                parseEvalState state program "" " left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores newlines" $ do
                parseEvalState state program "" "\n\nleft tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores whole-line comments" $ do
                parseEvalState state program "" "//Comment\n left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores in-line" $ do
                parseEvalState state program "" "/* Comment */\n left tape" `shouldParseStm` (MoveLeft "tape")

            it "ignores tabs" $ do
                parseEvalState state program "" "\tleft tape" `shouldParseStm` (MoveLeft "tape")

        context "interspersed with statements" $ do
            it "ignores whole line comments" $ do
                parseEvalState state program "" "left tape\n//Comment\n\n right tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores in-line comments" $ do
                parseEvalState state program "" "if /* Comment */ True { left tape }" `shouldParseStm` (If TRUE (MoveLeft "tape") [] Nothing)

        context "ignores whitespace after of statements" $ do
            it "ignores whitespace at the end of a statement" $ do
                parseEvalState state program "" "left tape  " `shouldParseStm` (MoveLeft "tape")

            it "ignores newlines at the end of a statement" $ do
                parseEvalState state program "" "left tape\n\n" `shouldParseStm` (MoveLeft "tape")

            it "ignores tabs" $ do
                parseEvalState state program "" "left tape\t" `shouldParseStm` (MoveLeft "tape")

        context "composition" $ do
            it "ignores tabs after the newline" $ do
                parseEvalState state program "" "left tape\n\tright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores tabs before the newline" $ do
                parseEvalState state program "" "left tape\t\nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores tabs alone on lines inbetween statements" $ do
                parseEvalState state program "" "left tape\n\t\nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

            it "ignores spaces alone on lines inbetween statements" $ do
                parseEvalState state program "" "left tape\n \nright tape" `shouldParseStm` (Comp (MoveLeft "tape") (MoveRight "tape"))

        context "whitespace nested in statements" $ do
            it "ignores newlines after an opening brace" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseEvalState state program "" "while True {\n\n left tape }" `shouldParseStm` expected

            it "ignores newlines before a closing brace" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseEvalState state program "" "while True { left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces" $ do
                let expected = While TRUE (MoveLeft "tape")
                parseEvalState state program "" "while True { \n\n left tape \n\n }" `shouldParseStm` expected

            it "ignores newlines after an opening brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseEvalState state program "" "while True {\n\n left tape \n\n right tape }" `shouldParseStm` expected

            it "ignores newlines after a closing brace when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseEvalState state program "" "while True { left tape \n\n right tape \n\n }" `shouldParseStm` expected

            it "ignores newlines before and after braces when composing" $ do
                let expected = While TRUE (Comp (MoveLeft "tape") (MoveRight "tape"))
                parseEvalState state program "" "while True { \n\n left tape \n\n right tape \n\n }" `shouldParseStm` expected
