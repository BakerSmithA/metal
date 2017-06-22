module Semantics.DenotationalSpec (denotationalSpec) where

import Semantics.Denotational
import State.Config as Config
import State.Env as Env
import State.Error
import State.Machine
import State.Program
import State.Tape
import Syntax.Tree
import TestHelper
import Test.Hspec
import Test.HUnit.Lang

type ProgResult = IO (Either RuntimeError (Machine Config))

-- A config where the tape holds the string 'abc' on the first three cells, and
-- the read-write head is over the second cell, i.e. over 'b'.
testConfig :: Config
testConfig = right (Config.fromString "abc")

-- Runs `s` in config with the empty environment.
evalSemantics :: Stm -> Config -> ProgResult
evalSemantics s config = runProgram (evalStm s (return config)) Env.initial

-- Asserts that when the semantics have finished being evaulated, the predicate
-- is true.
machShouldSatify :: ProgResult -> (Machine Config -> Bool) -> Expectation
machShouldSatify r predicate = do
     x <- r
     case x of
         Left  err  -> assertFailure ("Got error: " ++ (show err))
         Right mach -> mach `shouldSatisfy` predicate

-- Asserts that when the semantics have finished being evaulated, the machine
-- is in an state with a configuration which statisfied the preciate.
machShouldHaveConfig :: ProgResult -> (Config -> Bool) -> Expectation
machShouldHaveConfig r predicate = machShouldSatify r f where
    f (Inter c) = predicate c
    f _         = False

-- Asserts that when the semantics have finished being evauluated, the position
-- of the read-write head is in the given position.
shouldBeAt :: ProgResult -> Pos -> Expectation
shouldBeAt r p = machShouldHaveConfig r predicate where
    predicate c = pos c == p
    predicate _ = False

-- Asserts that the tape has the string `str` at the start of the tape.
shouldRead :: ProgResult -> [TapeSymbol] -> Expectation
shouldRead r syms = machShouldHaveConfig r predicate where
    predicate c = tapeShouldRead (tape c) syms
    predicate _ = False

-- Asserts that the machine halted in the accepting state.
shouldAccept :: ProgResult -> Expectation
shouldAccept r = machShouldSatify r (== HaltA)

-- Asserts that the machine halted in the rejecting state.
shouldReject :: ProgResult -> Expectation
shouldReject r = machShouldSatify r (== HaltR)

denotationalSpec :: Spec
denotationalSpec = do
    describe "evalStm" $ do
        leftSpec
        rightSpec
        writeSpec

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
        it "immediately accepts after evaluating an accept statement" $ do
            shouldAccept $ evalSemantics (Accept) testConfig

        it "performs no more statments after accepting" $ do
            let write = (Write (Literal '2'))
                comp  = Comp Accept write
            evalSemantics comp testConfig `shouldRead` "abc"
