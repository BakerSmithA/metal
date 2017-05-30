module Semantics.DenotationalSpec (denotationalSpec) where

import Syntax.Tree
import Semantics.State
import Semantics.Denotational
import Test.Hspec
import Test.HUnit.Lang

-- A configuration used for testing, where the tape is populated with the
-- string "Ab5#", the read/write head is at index 1 (at 'b'), and the
-- function environment is empty.
testConfig :: Config
testConfig = (tape, 1, initialEnvF) where
    tape 0 = 'A'
    tape 1 = 'b'
    tape 2 = '5'
    tape 3 = '#'

-- Asserts that the read/write head should be at position `p`.
shouldBeAt :: State -> Pos -> Expectation
shouldBeAt st p = case st of
    Inter (tape, pos, envf) -> pos `shouldBe` p
    _                       -> assertFailure "Expected inter-config"

-- Asserts that the symbol under the read/write head should be `s`.
shouldRead :: State -> TapeSymbol -> Expectation
shouldRead st s = case st of
    Inter (tape, pos, envf) -> tape pos `shouldBe` s
    _                       -> assertFailure "Expected inter-config"

-- Asserts that the machine rejected.
shouldReject :: (Config -> State) -> Config -> Expectation
shouldReject f config = case f config of
    HaltR        -> return ()
    HaltA        -> assertFailure "Expected halt reject, but accepted"
    Inter config -> assertFailure "Expected halt reject, but did not halt"

-- Asserts that the machine accepted.
shouldAccept :: (Config -> State) -> Config -> Expectation
shouldAccept f config = case f config of
    HaltA        -> return ()
    HaltR        -> assertFailure "Expected halt accept, but rejected"
    Inter config -> assertFailure "Expected halt accept, but did not halt"

denotationalSpec :: Spec
denotationalSpec = do
    stmValSpec

stmValSpec :: Spec
stmValSpec = do
    describe "stmVal" $ do
        context "head at zero position" $ do
            it "does not move left" $ do
                stmVal MoveLeft initialConfig `shouldBeAt` 0

        context "evaluating writing" $ do
            it "writes" $ do
                stmVal (Write 'x') testConfig `shouldRead` 'x'

        context "evauluating movement" $ do
            it "moves left" $ do
                stmVal MoveLeft testConfig `shouldBeAt` 0

            it "moves right" $ do
                stmVal MoveRight testConfig `shouldBeAt` 2

        context "evaluating halting" $ do
            it "rejects" $ do
                stmVal Reject `shouldReject` testConfig

            it "accepts" $ do
                stmVal Accept `shouldAccept` testConfig

        context "evaluating if statement" $ do
            it "performs the first branch" $ do
                let ifStm = If TRUE (Write '1')
                stmVal ifStm testConfig `shouldRead` '1'

            it "performs nothing if predicate is false" $ do
                let ifStm = If FALSE (Write '1')
                stmVal ifStm testConfig `shouldRead` 'b'

            it "reads the symbol under the read/write head" $ do
                let cond  = Eq Read (Literal 'b')
                    ifStm = If cond (Write '1')
                stmVal ifStm testConfig `shouldRead` '1'

        context "evaluating if-else statement" $ do
            it "performs the first branch" $ do
                let ifElse = IfElse TRUE (Write '1') (Write '2')
                stmVal ifElse testConfig `shouldRead` '1'

            it "performs the first branch" $ do
                let ifElse = IfElse FALSE (Write '1') (Write '2')
                stmVal ifElse testConfig `shouldRead` '2'

        context "evaluating while loop" $ do
            it "does not loop if the condition is false" $ do
                let loop = While FALSE (Write '1')
                stmVal loop testConfig `shouldRead` 'b'

            it "performs a loop" $ do
                -- Move left until at '#' character is reached.
                let cond = Not (Eq Read (Literal '#'))
                    loop = While cond MoveRight
                stmVal loop testConfig `shouldBeAt` 3
