module Semantics.StmSpec (stmSpec) where

import State.Config as Config
import State.Error
import Syntax.Tree
import TestHelper.Config
import TestHelper.Denotational
import Test.Hspec

-- Takes a function to construct a control structure (e.g. function, while-loop,
-- if-statement), given the body of the structure. Also takes an optional
-- statement to invoke the construct. The code below is executed and it is
-- checked the variable x if reset to have the value '1' after executing the
-- container.
--
--  let x = '1'
--  control_struct {
--      let x = '2'
--      write '#'
--  }
--  invoke
testResetVarEnv :: (Stm -> Stm) -> Maybe Stm -> Expectation
testResetVarEnv makeControlStruct invoke = do
    let outerVarDecl = VarDecl "x" (New $ S $ SymLit '1')
        innerVarDecl = VarDecl "x" (New $ S $ SymLit '2')
        write        = Write (Var "tape") (New $ SymLit '#')
        structBody   = Comp innerVarDecl write
        structDecl   = makeControlStruct structBody
        invoke'      = maybe structDecl (Comp structDecl) invoke
        comp         = Comp outerVarDecl invoke'
        testConfig   = Config.fromString "tape" "abc"
        result       = evalSemantics comp testConfig

    shouldReturnVar result "x" '1'

-- Takes a function to construct a control structure (e.g. function, while-loop,
-- if-statement), given the body of the structure. Also takes an
-- optional statement to invoke the construct. The code below is executed and
-- it is checked the function f is reset to have the body `right` after
-- executing the container.
--
--  func f x {
--      right
--  }
--  control_struct {
--      func f {
--          write '#'
--      }
--      f
--  }
--  invoke
testResetFuncEnv :: (Stm -> Stm) -> Maybe Stm -> Expectation
testResetFuncEnv makeControlStruct invoke = do
    let outerFBody = (MoveRight (Var "tape"))
        args       = [("x", SymType)]
        outerFDecl = FuncDecl "f" args outerFBody
        innerFBody = Write (Var "tape") (New $ SymLit '#')
        innerFDecl = FuncDecl "f" [] innerFBody
        fCall      = Call "f" []
        structBody = Comp innerFDecl fCall
        structDecl = makeControlStruct structBody
        invoke'    = maybe structDecl (Comp structDecl) invoke
        comp       = Comp outerFDecl invoke'
        testConfig = Config.fromString "tape" "abc"
        result     = evalSemantics comp testConfig

    shouldReturnFunc result "f" args outerFBody

stmSpec :: Spec
stmSpec = do
    describe "evalStm" $ do
        leftSpec
        rightSpec
        writeSpec
        acceptSpec
        rejectSpec
        ifSpec
        whileSpec
        varDeclSpec
        funcCallSpec
        compSpec
        printReadSpec
        printStrSpec

leftSpec :: Spec
leftSpec = do
    let testConfig = right (Config.fromString "tape" "abc")

    context "left" $ do
        it "moves the read-write head left" $ do
            shouldBeAt (evalSemantics (MoveLeft (Var "tape")) testConfig) "tape" 0

rightSpec :: Spec
rightSpec = do
    let testConfig = right (Config.fromString "tape" "abc")

    context "right" $ do
        it "moves the read-write head right" $ do
            shouldBeAt (evalSemantics ((MoveRight (Var "tape"))) testConfig) "tape" 2

writeSpec :: Spec
writeSpec = do
    let testConfig = right (Config.fromString "tape" "abc")

    context "right" $ do
        it "writes to the cell under the read-write head" $ do
            shouldRead (evalSemantics (Write (Var "tape") (New $ SymLit '2')) testConfig) "tape" "a2c"

acceptSpec :: Spec
acceptSpec = do
    let testConfig = right (Config.fromString "tape" "abc")

    context "accept" $ do
        it "accepts after evaluating an accept statement" $ do
            shouldAccept $ evalSemantics (Accept) testConfig

rejectSpec :: Spec
rejectSpec = do
    let testConfig = right (Config.fromString "tape" "abc")

    context "reject" $ do
        it "rejects after evaluating an accept statement" $ do
            shouldReject $ evalSemantics (Reject) testConfig

ifSpec :: Spec
ifSpec = do
    let testConfig = right (Config.fromString "tape" "abc")

    context "evaluating a single if-statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write (Var "tape") (New $ SymLit '1')) [] Nothing
            shouldRead (evalSemantics ifStm testConfig) "tape" "a1c"

        it "performs nothing if predicate is false" $ do
            let ifStm = If FALSE (Write (Var "tape") (New $ SymLit '1')) [] Nothing
            shouldRead (evalSemantics ifStm testConfig) "tape" "abc"

        it "resets the variable environment after executing a branch" $ do
            let makeIf body = If TRUE body [] Nothing
            testResetVarEnv makeIf Nothing

        it "resets the function environment after executing a branch" $ do
            let makeIf body = If TRUE body [] Nothing
            testResetFuncEnv makeIf Nothing

    context "evaluating an if-elseif statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write (Var "tape") (New $ SymLit '1')) [(TRUE, Write (Var "tape") (New $ SymLit '2'))] Nothing
            shouldRead (evalSemantics ifStm testConfig) "tape" "a1c"

        it "performs the second branch" $ do
            let ifStm = If FALSE (Write (Var "tape") (New $ SymLit '1')) [(TRUE, Write (Var "tape") (New $ SymLit '2'))] Nothing
            shouldRead (evalSemantics ifStm testConfig) "tape" "a2c"

        it "performs the third branch" $ do
            let ifStm = If FALSE (Write (Var "tape") (New $ SymLit '1')) [(FALSE, Write (Var "tape") (New $ SymLit '2')), (TRUE, Write (Var "tape") (New $ SymLit '3'))] Nothing
            shouldRead (evalSemantics ifStm testConfig) "tape" "a3c"

        it "resets the variable environment after executing a branch" $ do
            let makeIf body = If FALSE (Write (Var "tape") (New $ SymLit '1')) [(TRUE, body)] Nothing
            testResetVarEnv makeIf Nothing

        it "resets the function environment after executing a branch" $ do
            let makeIf body = If FALSE (Write (Var "tape") (New $ SymLit '1')) [(TRUE, body)] Nothing
            testResetFuncEnv makeIf Nothing

    context "evaluating an if-elseif-else statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write (Var "tape") (New $ SymLit '1')) [(TRUE, Write (Var "tape") (New $ SymLit '2'))] (Just (Write (Var "tape") (New $ SymLit '3')))
            shouldRead (evalSemantics ifStm testConfig) "tape" "a1c"

        it "performs the second branch" $ do
            let ifStm = If FALSE (Write (Var "tape") (New $ SymLit '1')) [(TRUE, Write (Var "tape") (New $ SymLit '2'))] (Just (Write (Var "tape") (New $ SymLit '3')))
            shouldRead (evalSemantics ifStm testConfig) "tape" "a2c"

        it "performs the else branch" $ do
            let ifStm = If FALSE (Write (Var "tape") (New $ SymLit '1')) [(FALSE, Write (Var "tape") (New $ SymLit '2'))] (Just (Write (Var "tape") (New $ SymLit '3')))
            shouldRead (evalSemantics ifStm testConfig) "tape" "a3c"

        it "resets the variable environment after executing a branch" $ do
            let makeIf body = If FALSE (Write (Var "tape") (New $ SymLit '1')) [] (Just body)
            testResetVarEnv makeIf Nothing

        it "resets the function environment after executing a branch" $ do
            let makeIf body = If FALSE (Write (Var "tape") (New $ SymLit '1')) [] (Just body)
            testResetFuncEnv makeIf Nothing

whileSpec :: Spec
whileSpec = do
    let testConfig = Config.fromString "tape" "Ab5#"

    context "evaluating while loop" $ do
        it "does not loop if the condition is false" $ do
            let loop = While FALSE (Write (Var "tape") (New $ SymLit '1'))
            shouldRead (evalSemantics loop testConfig) "tape" "Ab5#"

        it "performs a loop" $ do
            -- Move right until a '#' character is reached, overwriting each
            -- character with 'X'.
            let cond   = Not (Eq (New $ Read (Var "tape")) (New $ SymLit '#'))
                comp   = Comp (Write (Var "tape") (New $ SymLit 'X')) (MoveRight (Var "tape"))
                loop   = While cond comp
                result = evalSemantics loop testConfig

            shouldBeAt result "tape" 3
            shouldRead result "tape" "XXX#"

        it "resets the variable environment after executing a branch" $ do
            let cond        = Not (Eq (New $ Read (Var "tape")) (New $ SymLit '#'))
                makeIf body = While cond (Comp (MoveRight (Var "tape")) body)

            testResetVarEnv makeIf Nothing

        it "resets the function environment after executing a branch" $ do
            let cond        = Not (Eq (New $ Read (Var "tape")) (New $ SymLit '#'))
                makeIf body = While cond (Comp (MoveRight (Var "tape")) body)

            testResetFuncEnv makeIf Nothing

        it "breaks by rejecting" $ do
            let loop = While TRUE Reject
            shouldReject (evalSemantics loop testConfig)

        it "breaks by accepting" $ do
            let loop = While TRUE Accept
            shouldAccept (evalSemantics loop testConfig)

varDeclSpec :: Spec
varDeclSpec = do
    let testConfig = Config.fromString "tape" "abc"

    context "evaluating a variable declaration" $ do
        it "adds the variable to the environment" $ do
            let decl   = VarDecl "y" (New $ S $ SymLit '1')
                ifStm  = If (Eq (Var "y") (New $ SymLit '1')) (Write (Var "tape") (New $ SymLit '#')) [] Nothing
                comp   = Comp decl ifStm

            shouldRead (evalSemantics comp testConfig) "tape" "#bc"

funcCallSpec :: Spec
funcCallSpec = do
    let testConfig = Config.fromString "tape" "abc"

    context "evaluating a function call" $ do
        it "performs the function" $ do
            let decl = FuncDecl "f" [] (MoveRight (Var "tape"))
                call = Call "f" []
                comp = Comp decl call
            shouldBeAt (evalSemantics comp testConfig) "tape" 1

        it "fails if the function has not been defined" $ do
            let call = Call "f" []
            evalSemantics call testConfig `shouldThrow` (== UndefFunc "f")

        it "resets the variable environment after executing a function" $ do
            testResetVarEnv (FuncDecl "g" []) (Just (Call "g" []))

        it "resets the function environment after executing a function" $ do
            testResetFuncEnv (FuncDecl "g" []) (Just (Call "g" []))

        it "evalutes a function with arguments" $ do
            -- The statement used in the test is:
            --
            --  func x y {
            --      if (x == '1') and (y == '2') {
            --          left
            --      } else {
            --          right
            --      }
            --  }
            --
            -- The function is then called with the following arguments:
            --  f '1' '2'
            --  f '1' '3'
            let boolX    = Eq (Var "x") (New $ SymLit '1')
                boolY    = Eq (Var "y") (New $ SymLit '2')
                boolAnd  = And boolX boolY
                ifStm    = If boolAnd (MoveLeft (Var "tape")) [] (Just (MoveRight (Var "tape")))
                args     = [("x", SymType), ("y", SymType)]
                funcDecl = FuncDecl "f" args ifStm
                call1    = Comp funcDecl (Call "f" [New $ S $ SymLit '1', New $ S $ SymLit '2'])
                call2    = Comp funcDecl (Call "f" [New $ S $ SymLit '1', New $ S $ SymLit '3'])
                config   = right (Config.fromString "tape" "abc")

            shouldBeAt (evalSemantics call1 config) "tape" 0
            shouldBeAt (evalSemantics call2 config) "tape" 2

        it "throws an error if the number of arguments is incorrect" $ do
            let args     = [("a", SymType), ("b", SymType)]
                funcDecl = FuncDecl "f" args Accept
                comp     = Comp funcDecl (Call "f" [New $ S $ SymLit '1'])
                expected = WrongNumArgs "f" args [New $ S $ SymLit '1']
            evalSemantics comp testConfig `shouldThrow` (== expected)

        it "evaluates a recursive function" $ do
            -- The statement used in the test is shown below. This function
            -- moves right until a '#' is encountered. When a '#' is found,
            -- the program accepts. If the end of the input is found
            -- (i.e. ' ') the program rejects.
            --
            --  func f {
            --      if read == '#' {
            --          accept
            --      } else if read == ' ' {
            --          reject
            --      } else {
            --          right
            --          f
            --      }
            --  }
            --  f
            let b1            = Eq (New $ Read (Var "tape")) (New $ SymLit '#')
                b2            = Eq (New $ Read (Var "tape")) (New $ SymLit ' ')
                elseIfClauses = [(b2, Reject)]
                elseClause    = Just (Comp (MoveRight (Var "tape")) (Call "f" []))
                ifStm         = If b1 Accept elseIfClauses elseClause
                funcDecl      = FuncDecl "f" [] ifStm
                termConfig    = Config.fromString "tape" "abc#"
                nonTermConfig = Config.fromString "tape" "abc"
                comp          = Comp funcDecl (Call "f" [])

            shouldAccept (evalSemantics comp termConfig)
            shouldReject (evalSemantics comp nonTermConfig)

        it "modifies a reference to a tape" $ do
            let declTape = VarDecl "tape1" (New $ T $ TapeLit "xyz")
                body     = Write (Var "inputTape") (New $ SymLit '1')
                funcDecl = FuncDecl "modifyTape" [("inputTape", TapeType)] body
                comp     = Comp declTape (Comp funcDecl (Call "modifyTape" [Var "tape1"]))
                result   = evalSemantics comp Config.empty

            shouldRead result "tape1" "1yz"

        it "removes the tape reference once the function is exited" $ do
            let declTape = VarDecl "tape1" (New $ T $ TapeLit "xyz")
                body     = Write (Var "inputTape") (New $ SymLit '1')
                funcDecl = FuncDecl "modifyTape" [("inputTape", TapeType)] body
                comp1    = Comp declTape (Comp funcDecl (Call "modifyTape" [Var "tape1"]))
                -- Attempt to use inputTape which was only declared as an argument of the function.
                comp2    = Comp comp1 (Write (Var "inputTape") (New $ SymLit 'a'))
                result   = evalSemantics comp2 Config.empty

            result `shouldThrow` (== UndefTape "inputTape")

        it "removes function variables once the function exited" $ do
            let declSym  = VarDecl "x" (New $ S $ SymLit '1')
                body     = PrintStr "hello"
                funcDecl = FuncDecl "f" [("y", SymType)] body
                comp1    = Comp declSym (Comp funcDecl (Call "f" [Var "x"]))
                -- Attempt to use y which was only declared as an argument of the function.
                comp2    = Comp comp1 (VarDecl "new" (Var "y"))
                result   = evalSemantics comp2 Config.empty

            result `shouldThrow` (== UndefVar "y")

        it "fails if incorrect types are supplied" $ do
            let funcDecl = FuncDecl "f" [("x", TapeType)] (PrintStr "hello")
                call     = Call "f" [New $ S $ SymLit 'x']
                comp     = Comp funcDecl call
                result   = evalSemantics comp Config.empty

            result `shouldThrow` (== MismatchedTypes "x" "f" TapeType (New $ S $ SymLit 'x'))

        it "accepts a tape literal as argument" $ do
            let funcDecl = FuncDecl "f" [("t", TapeType)] (PrintRead (Var "t"))
                call     = Call "f" [New $ T $ TapeLit "xyz"]
                comp     = Comp funcDecl call
                result   = evalSemantics comp Config.empty

            result `shouldOutput` ["x"]

        it "removes tape literals after function exited" $ do
            let body     = Write (Var "inputTape") (New $ SymLit '1')
                funcDecl = FuncDecl "modifyTape" [("inputTape", TapeType)] body
                comp1    = Comp funcDecl (Call "modifyTape" [(New $ T $ TapeLit "xyz")])
                -- Attempt to use inputTape which was only declared as an argument of the function.
                comp2    = Comp comp1 (Write (Var "inputTape") (New $ SymLit 'a'))
                result   = evalSemantics comp2 Config.empty

            result `shouldThrow` (== UndefTape "inputTape")

compSpec :: Spec
compSpec = do
    let testConfig = Config.fromString "tape" "abc"

    context "evaluating a function composition" $ do
        it "composes two statements 1" $ do
            let comp   = Comp (MoveRight (Var "tape")) (Write (Var "tape") (New $ SymLit '#'))
                result = evalSemantics comp testConfig
            shouldBeAt result "tape" 1
            shouldRead result "tape" "a#c"

        it "composes two statements 2" $ do
            let ifStm  = If (Eq ((New $ Read (Var "tape"))) (New $ SymLit 'b')) (Write (Var "tape") (New $ SymLit '#')) [] Nothing
                comp   = Comp (MoveRight (Var "tape")) ifStm
                result = evalSemantics comp testConfig
            shouldRead result "tape" "a#c"

printReadSpec :: Spec
printReadSpec = do
    let testConfig = Config.fromString "tape" "abc"

    context "evaluating printing the current symbol" $ do
        it "prints the current symbol" $ do
            let result = evalSemantics (PrintRead (Var "tape")) testConfig
            result `shouldOutput` ["a"]

        it "prints multiple symbols in the correct order" $ do
            let comp   = Comp (PrintRead (Var "tape")) (Comp (PrintRead (Var "tape")) (PrintRead (Var "tape")))
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a", "a", "a"]

        it "prints multiple symbols in the correct order using movement" $ do
            let comp   = Comp (PrintRead (Var "tape")) (Comp (MoveRight (Var "tape")) (Comp (PrintRead (Var "tape")) (Comp (MoveRight (Var "tape")) (PrintRead (Var "tape")))))
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a", "b", "c"]

    context "evaluating printing the current symbol using an if" $ do
        it "prints using an if" $ do
            let comp   = If TRUE (PrintRead (Var "tape")) [] Nothing
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a"]

        it "does not print anything before the if multiple times" $ do
            let ifStm  = If TRUE (PrintRead (Var "tape")) [] Nothing
                comp   = Comp (PrintRead (Var "tape")) ifStm
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a", "a"]

    context "evaluating printing the current symbol using a loop" $ do
        it "prints multiple using a loop" $ do
            let comp = While (Not (Eq (New $ Read (Var "tape")) (New $ SymLit ' '))) (Comp (PrintRead (Var "tape")) (MoveRight (Var "tape")))
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a", "b", "c"]

        it "does not print anything before the loop multiple times" $ do
            let loop = While (Not (Eq (New $ Read (Var "tape")) (New $ SymLit ' '))) (Comp (PrintRead (Var "tape")) (MoveRight (Var "tape")))
                comp = Comp (PrintRead (Var "tape")) loop
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a", "a", "b", "c"]

    context "evaluating printing using a function" $ do
        it "prints multiple using a function" $ do
            let decl   = FuncDecl "f" [] (PrintRead (Var "tape"))
                comp   = Comp decl (Call "f" [])
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a"]

        it "does not print anything before the function multiple times" $ do
            let decl   = FuncDecl "f" [] (PrintRead (Var "tape"))
                comp   = Comp (PrintRead (Var "tape")) (Comp decl (Call "f" []))
                result = evalSemantics comp testConfig
            result `shouldOutput` ["a", "a"]

printStrSpec :: Spec
printStrSpec = do
    let testConfig = Config.fromString "tape" "abc"

    context "evaluating printing the current symbol" $ do
        it "prints a string" $ do
            let result = evalSemantics (PrintStr "hello") testConfig
            result `shouldOutput` ["hello"]

        it "prints multiple strings in the correct order" $ do
            let comp   = Comp (PrintStr "1") (Comp (PrintStr "2") (PrintStr "3"))
                result = evalSemantics comp testConfig
            result `shouldOutput` ["1", "2", "3"]
