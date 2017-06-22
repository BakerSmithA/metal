module Semantics.DenotationalSpec (denotationalSpec) where

import Semantics.Denotational
import State.Config as Config
import State.Env as Env
import State.Error
import State.Machine
import State.Program
import State.Tape
import Syntax.Tree
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

-- Asserts that when the semantics have finished being evauluated, the position
-- of the read-write head is in the given position.
shouldBeAt :: ProgResult -> Pos -> Expectation
shouldBeAt r p = r `machShouldSatify` predicate where
    predicate (Inter c) = pos c == p
    predicate _         = False

denotationalSpec :: Spec
denotationalSpec = do
    describe "evalStm" $ do
        leftSpec

leftSpec :: Spec
leftSpec = do
    context "left" $ do
        it "moves the read-write head left" $ do
            evalSemantics (MoveLeft) testConfig `shouldBeAt` 0
