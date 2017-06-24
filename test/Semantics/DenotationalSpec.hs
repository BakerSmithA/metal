module Semantics.DenotationalSpec
( derivedSymbolValSpec
, bexpValSpec
, denotationalSpec
) where

import Semantics.Denotational
import State.Program
import State.Config as Config
import State.Env as Env
import State.Error
import State.Tape()
import Syntax.Tree
import TestHelper.Denotational
import Test.Hspec hiding (shouldContain, shouldThrow)

derivedSymbolValSpec :: Spec
derivedSymbolValSpec = do
    let testConfig = right (Config.fromString "abc")
        testEnv    = Env.addVar "x" '1' Env.empty

    describe "derivedSymbolVal" $ do
        it "reads the symbol under the read-write head" $ do
            let result = evalDerivedSymbol Read testConfig testEnv
            result `shouldContain` 'b'

        it "returns the literal" $ do
            let result = evalDerivedSymbol (Literal 'm') testConfig testEnv
            result `shouldContain` 'm'

        it "returns the value of a variable" $ do
            let result = evalDerivedSymbol (Var "x") testConfig testEnv
            result `shouldContain` '1'

        it "fails if the variable is not defined" $ do
            let result = evalDerivedSymbol (Var "undef") testConfig testEnv
            result `shouldThrow` (UndefVar "undef")

bexpValSpec :: Spec
bexpValSpec = do
    let testConfig = right (Config.fromString "abc")
        testEnv    = Env.addVar "x" '1' Env.empty
    describe "bexpVal" $ do
        it "evaluates TRUE" $ do
            let result = evalBexp TRUE testConfig testEnv
            result `shouldContain` True

        it "evaluates FALSE" $ do
            let result = evalBexp FALSE testConfig testEnv
            result `shouldContain` False

        it "evaluates not" $ do
            let result = evalBexp (Not TRUE) testConfig testEnv
            result `shouldContain` False

        it "evaluates and" $ do
            let ff = evalBexp (And FALSE FALSE) testConfig testEnv
                ft = evalBexp (And FALSE TRUE) testConfig testEnv
                tf = evalBexp (And TRUE FALSE) testConfig testEnv
                tt = evalBexp (And TRUE TRUE) testConfig testEnv
            ff `shouldContain` False
            ft `shouldContain` False
            tf `shouldContain` False
            tt `shouldContain` True

        it "evaluates or" $ do
            let ff = evalBexp (Or FALSE FALSE) testConfig testEnv
                ft = evalBexp (Or FALSE TRUE) testConfig testEnv
                tf = evalBexp (Or TRUE FALSE) testConfig testEnv
                tt = evalBexp (Or TRUE TRUE) testConfig testEnv
            ff `shouldContain` False
            ft `shouldContain` True
            tf `shouldContain` True
            tt `shouldContain` True

        it "evaluates <=" $ do
            let b1      = Le (Read) (Literal 'c') -- The current symbol is 'b'.
                b2      = Le (Read) (Literal 'a')
                result1 = evalBexp b1 testConfig testEnv
                result2 = evalBexp b2 testConfig testEnv
            result1 `shouldContain` True
            result2 `shouldContain` False

        it "evaluates ==" $ do
            let b1      = Eq (Read) (Literal 'b') -- The current symbol is 'b'.
                b2      = Eq (Read) (Literal '#')
                result1 = evalBexp b1 testConfig testEnv
                result2 = evalBexp b2 testConfig testEnv
            result1 `shouldContain` True
            result2 `shouldContain` False

denotationalSpec :: Spec
denotationalSpec = do
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

leftSpec :: Spec
leftSpec = do
    let testConfig = right (Config.fromString "abc")
    context "left" $ do
        it "moves the read-write head left" $ do
            evalSemantics (MoveLeft) testConfig `shouldBeAt` 0

rightSpec :: Spec
rightSpec = do
    let testConfig = right (Config.fromString "abc")
    context "right" $ do
        it "moves the read-write head right" $ do
            evalSemantics (MoveRight) testConfig `shouldBeAt` 2

writeSpec :: Spec
writeSpec = do
    let testConfig = right (Config.fromString "abc")
    context "right" $ do
        it "writes to the cell under the read-write head" $ do
            evalSemantics (Write (Literal '2')) testConfig `shouldRead` "a2c"

acceptSpec :: Spec
acceptSpec = do
    let testConfig = right (Config.fromString "abc")
    context "accept" $ do
        it "accepts after evaluating an accept statement" $ do
            shouldAccept $ evalSemantics (Accept) testConfig

rejectSpec :: Spec
rejectSpec = do
    let testConfig = right (Config.fromString "abc")
    context "reject" $ do
        it "rejects after evaluating an accept statement" $ do
            shouldReject $ evalSemantics (Reject) testConfig

ifSpec :: Spec
ifSpec = do
    let testConfig = right (Config.fromString "abc")
    context "evaluating a single if-statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write (Literal '1')) [] Nothing
            evalSemantics ifStm testConfig `shouldRead` "a1c"

        it "performs nothing if predicate is false" $ do
            let ifStm = If FALSE (Write (Literal '1')) [] Nothing
            evalSemantics ifStm testConfig `shouldRead` "abc"

    context "evaluating an if-elseif statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write (Literal '1')) [(TRUE, Write (Literal '2'))] Nothing
            evalSemantics ifStm testConfig `shouldRead` "a1c"

        it "performs the second branch" $ do
            let ifStm = If FALSE (Write (Literal '1')) [(TRUE, Write (Literal '2'))] Nothing
            evalSemantics ifStm testConfig `shouldRead` "a2c"

        it "performs the third branch" $ do
            let ifStm = If FALSE (Write (Literal '1')) [(FALSE, Write (Literal '2')), (TRUE, Write (Literal '3'))] Nothing
            evalSemantics ifStm testConfig `shouldRead` "a3c"

    context "evaluating an if-elseif-else statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write (Literal '1')) [(TRUE, Write (Literal '2'))] (Just (Write (Literal '3')))
            evalSemantics ifStm testConfig `shouldRead` "a1c"

        it "performs the second branch" $ do
            let ifStm = If FALSE (Write (Literal '1')) [(TRUE, Write (Literal '2'))] (Just (Write (Literal '3')))
            evalSemantics ifStm testConfig `shouldRead` "a2c"

        it "performs the else branch" $ do
            let ifStm = If FALSE (Write (Literal '1')) [(FALSE, Write (Literal '2'))] (Just (Write (Literal '3')))
            evalSemantics ifStm testConfig `shouldRead` "a3c"

whileSpec :: Spec
whileSpec = do
    let testConfig = Config.fromString "Ab5#"
    context "evaluating while loop" $ do
        it "does not loop if the condition is false" $ do
            let loop = While FALSE (Write (Literal '1'))
            evalSemantics loop testConfig `shouldRead` "Ab5#"

        it "performs a loop" $ do
            -- Move right until a '#' character is reached.
            let cond = Not (Eq Read (Literal '#'))
                loop = While cond MoveRight
            evalSemantics loop testConfig `shouldBeAt` 3

        -- it "breaks by rejecting" $ do
        --     let loop = While TRUE Reject
        --     shouldReject (evalSemantics loop testConfig)

        -- it "breaks by accepting" $ do
        --     let loop = While TRUE Accept
        --     shouldAccept (evalSemantics loop testConfig)

varDeclSpec :: Spec
varDeclSpec = do
    let testConfig = Config.fromString "abc"
    context "evaluating a variable declaration" $ do
        -- it "adds the variable to the environment" $ do
        --     let decl   = VarDecl "y" (Literal '1')
        --         prog   = evalStm decl (return testConfig)
        --         result = runProgram (derivedSymbolVal (Var "y") prog) Env.empty
        --     result `shouldContain` '1'

        it "adds the variable to the environment" $ do
            let decl   = VarDecl "y" (Literal '1')
                ifStm  = If (Eq (Var "y") (Literal '1')) (Write (Literal '#')) [] Nothing
                comp   = Comp decl ifStm
            evalSemantics comp testConfig `shouldRead` "#bc"

funcCallSpec :: Spec
funcCallSpec = do
    let testConfig = Config.fromString "abc"
    context "evaluating a function call" $ do
        it "performs the function" $ do
            let decl = FuncDecl "f" MoveRight
                call = Call "f"
                comp = Comp decl call
            evalSemantics comp testConfig `shouldBeAt` 1

compSpec :: Spec
compSpec = do
    let testConfig = Config.fromString "abc"
    context "evaluating a function composition" $ do
        it "composes two statements 1" $ do
            let comp   = Comp MoveRight (Write (Literal '#'))
                result = evalSemantics comp testConfig
            result `shouldBeAt` 1
            result `shouldRead` "a#c"

        it "composes two statements 2" $ do
            let ifStm = If (Eq (Read) (Literal 'b')) (Write (Literal '#')) [] Nothing
                comp  = Comp MoveRight ifStm
            evalSemantics comp testConfig `shouldRead` "a#c"
