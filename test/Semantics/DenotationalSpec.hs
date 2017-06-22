module Semantics.DenotationalSpec
( derivedSymbolValSpec
, bexpValSpec
, denotationalSpec
) where

import Semantics.Denotational
import State.Config as Config
import State.Env as Env
import State.Error
import State.Machine
import State.Program
import State.Tape
import Syntax.Tree
import TestHelper
import Test.Hspec hiding (shouldContain, shouldSatify, shouldThrow)
import Test.HUnit.Lang

type ProgResult a     = IO (Either RuntimeError (Machine a))
type ProgResultConfig = ProgResult Config

-- A config where the tape holds the string 'abc' on the first three cells, and
-- the read-write head is over the second cell, i.e. over 'b'.
testConfig :: Config
testConfig = right (Config.fromString "abc")

-- An environment where the variable "x" maps to the tape symbol '1'.
testEnv :: Env
testEnv = Env.addVar "x" '1' Env.empty

-- Runs `derivedSymbolVal` with `sym` in the given config and environment.
evalDerivedSymbol :: DerivedSymbol -> Config -> Env -> ProgResult TapeSymbol
evalDerivedSymbol sym config = runProgram (derivedSymbolVal sym (return config))

-- Runs `bexpVal` with `b` in the given config and environment.
evalBexp :: Bexp -> Config -> Env -> ProgResult Bool
evalBexp b config = runProgram (bexpVal b (return config))

-- Runs `s` in config with the empty environment.
evalSemantics :: Stm -> Config -> ProgResultConfig
evalSemantics s config = runProgram (evalStm s (return config)) Env.empty

-- Asserts that a runtime error was thrown.
shouldThrow :: (Show a) => ProgResult a -> RuntimeError -> Expectation
shouldThrow r expected = do
    x <- r
    case x of
        Left err   -> err `shouldBe` expected
        Right mach -> assertFailure ("Expected err, got machine: " ++ (show mach))

-- Asserts that when the semantics have finished being evaulated, the resulting
-- machine satisfies the given predicate.
machShouldSatify :: (Eq a, Show a) => ProgResult a -> (Machine a -> Bool) -> Expectation
machShouldSatify r predicate = do
     x <- r
     case x of
         Left  err  -> assertFailure ("Expected machine, got error: " ++ (show err))
         Right mach -> mach `shouldSatisfy` predicate

-- Asserts that when the semantics have finished being evaulated, the value
-- wrapped in the machine satisfies the predicate.
shouldSatify :: (Eq a, Show a) => ProgResult a -> (a -> Bool) -> Expectation
shouldSatify r predicate = machShouldSatify r f where
    f = machine False False predicate

-- Asserts that when the semantics have finished being evaulated, the machine
-- contains the given value.
shouldContain :: (Eq a, Show a) => ProgResult a -> a -> Expectation
shouldContain r sym = shouldSatify r (== sym)

-- Asserts that when the semantics have finished being evauluated, the position
-- of the read-write head is in the given position.
shouldBeAt :: ProgResultConfig -> Pos -> Expectation
shouldBeAt r p = shouldSatify r predicate where
    predicate c = pos c == p
    predicate _ = False

-- Asserts that the tape has the string `str` at the start of the tape.
shouldRead :: ProgResultConfig -> [TapeSymbol] -> Expectation
shouldRead r syms = shouldSatify r predicate where
    predicate c = tapeShouldRead (tape c) syms
    predicate _ = False

-- Asserts that the machine halted in the accepting state.
shouldAccept :: ProgResultConfig -> Expectation
shouldAccept r = machShouldSatify r (== HaltA)

-- Asserts that the machine halted in the rejecting state.
shouldReject :: ProgResultConfig -> Expectation
shouldReject r = machShouldSatify r (== HaltR)

derivedSymbolValSpec :: Spec
derivedSymbolValSpec = do
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

leftSpec :: Spec
leftSpec = do
    context "left" $ do
        it "moves the read-write head left" $ do
            evalSemantics (MoveLeft) testConfig `shouldBeAt` 0

rightSpec :: Spec
rightSpec = do
    context "right" $ do
        it "moves the read-write head right" $ do
            evalSemantics (MoveRight) testConfig `shouldBeAt` 2

writeSpec :: Spec
writeSpec = do
    context "right" $ do
        it "writes to the cell under the read-write head" $ do
            evalSemantics (Write (Literal '2')) testConfig `shouldRead` "a2c"

acceptSpec :: Spec
acceptSpec = do
    context "accept" $ do
        it "accepts after evaluating an accept statement" $ do
            shouldAccept $ evalSemantics (Accept) testConfig

rejectSpec :: Spec
rejectSpec = do
    context "reject" $ do
        it "rejects after evaluating an accept statement" $ do
            shouldReject $ evalSemantics (Reject) testConfig
