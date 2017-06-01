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
    tape _ = ' '

-- A configuration used for testing that is not terminated by a '#' symbol. The
-- tape is populated with the string "Ab5x", the read/write head is at index 1
-- (at 'b'), and the function environment is empty.
nonTerminatedTestConfig :: Config
nonTerminatedTestConfig = (tape', pos, envF) where
    tape' = update 3 'x' tape
    (tape, pos, envF) = testConfig

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
    derivedSymbolValSpec
    bexpValSpec
    evalIfSpec
    evalStmSpec

derivedSymbolValSpec :: Spec
derivedSymbolValSpec = do
    describe "derivedSymbolVal" $ do
        it "evaluates the value of a literal" $ do
            derivedSymbolVal (Literal 'x') testConfig `shouldBe` 'x'

        it "evaluates reading the current symbol" $ do
            derivedSymbolVal Read testConfig `shouldBe` 'b'

bexpValSpec :: Spec
bexpValSpec = do
    describe "bexpVal" $ do
        it "evaluates TRUE" $ do
            bexpVal TRUE testConfig `shouldBe` True

        it "evaulates FALSE" $ do
            bexpVal FALSE testConfig `shouldBe` False

        it "evaluates negation of true" $ do
            bexpVal (Not TRUE) testConfig `shouldBe` False

        it "evaulates negation of false" $ do
            bexpVal (Not FALSE) testConfig `shouldBe` True

        it "evaulates FALSE & FALSE" $ do
            bexpVal (And FALSE FALSE) testConfig `shouldBe` False

        it "evaulates FALSE & TRUE" $ do
            bexpVal (And FALSE TRUE) testConfig `shouldBe` False

        it "evaulates TRUE & FALSE" $ do
            bexpVal (And TRUE FALSE) testConfig `shouldBe` False

        it "evaulates TRUE & TRUE" $ do
            bexpVal (And TRUE TRUE) testConfig `shouldBe` True

        it "evaulates TRUE & TRUE & FALSE" $ do
            bexpVal (And (And TRUE TRUE) FALSE) testConfig `shouldBe` False

        it "evaluates == to be true" $ do
            bexpVal (Eq (Literal 'b') (Read)) testConfig `shouldBe` True

        it "evaluates == to be false" $ do
            bexpVal (Eq (Literal 'a') (Literal 'b')) testConfig `shouldBe` False

        it "evaluates <= to be true if greater than" $ do
            bexpVal (Le (Literal ' ') (Literal '!')) testConfig `shouldBe` True
            bexpVal (Le (Literal 'B') (Literal 'C')) testConfig `shouldBe` True
            bexpVal (Le (Literal '=') (Literal '?')) testConfig `shouldBe` True

        it "evaluates <= to be true if equal" $ do
            bexpVal (Le (Literal ' ') (Literal ' ')) testConfig `shouldBe` True
            bexpVal (Le (Literal 'A') (Literal 'A')) testConfig `shouldBe` True
            bexpVal (Le (Literal '=') (Literal '=')) testConfig `shouldBe` True

        it "evaluates <= to be false" $ do
            bexpVal (Le (Literal '!') (Literal ' ')) testConfig `shouldBe` False
            bexpVal (Le (Literal 'C') (Literal 'B')) testConfig `shouldBe` False
            bexpVal (Le (Literal '?') (Literal '=')) testConfig `shouldBe` False

evalIfSpec :: Spec
evalIfSpec = describe "evalIf" $ do
    context "evaluating a single if statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write '1') [] Nothing
            evalStm ifStm testConfig `shouldRead` '1'

        it "performs nothing if predicate is false" $ do
            let ifStm = If FALSE (Write '1') [] Nothing
            evalStm ifStm testConfig `shouldRead` 'b'

    context "evaluating an if-elseif statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write '1') [(TRUE, Write '2')] Nothing
            evalStm ifStm testConfig `shouldRead` '1'

        it "performs the second branch" $ do
            let ifStm = If FALSE (Write '1') [(TRUE, Write '2')] Nothing
            evalStm ifStm testConfig `shouldRead` '2'

        it "performs the third branch" $ do
            let ifStm = If FALSE (Write '1') [(FALSE, Write '2'), (TRUE, Write '3')] Nothing
            evalStm ifStm testConfig `shouldRead` '3'

    context "evaluating an if-elseif-else statement" $ do
        it "performs the first branch" $ do
            let ifStm = If TRUE (Write '1') [(TRUE, Write '2')] (Just (Write '3'))
            evalStm ifStm testConfig `shouldRead` '1'

        it "performs the second branch" $ do
            let ifStm = If FALSE (Write '1') [(TRUE, Write '2')] (Just (Write '3'))
            evalStm ifStm testConfig `shouldRead` '2'

        it "performs the else branch" $ do
            let ifStm = If FALSE (Write '1') [(FALSE, Write '2')] (Just (Write '3'))
            evalStm ifStm testConfig `shouldRead` '3'

evalStmSpec :: Spec
evalStmSpec = do
    describe "evalStm" $ do
        context "head at zero position" $ do
            it "does not move left" $ do
                evalStm MoveLeft initialConfig `shouldBeAt` 0

        context "evaluating writing" $ do
            it "writes" $ do
                evalStm (Write 'x') testConfig `shouldRead` 'x'

        context "evauluating movement" $ do
            it "moves left" $ do
                evalStm MoveLeft testConfig `shouldBeAt` 0

            it "moves right" $ do
                evalStm MoveRight testConfig `shouldBeAt` 2

        context "evaluating halting" $ do
            it "rejects" $ do
                evalStm Reject `shouldReject` testConfig

            it "accepts" $ do
                evalStm Accept `shouldAccept` testConfig

        context "evaluating while loop" $ do
            it "does not loop if the condition is false" $ do
                let loop = While FALSE (Write '1')
                evalStm loop testConfig `shouldRead` 'b'

            it "performs a loop" $ do
                -- Move left until at '#' character is reached.
                let cond = Not (Eq Read (Literal '#'))
                    loop = While cond MoveRight
                evalStm loop testConfig `shouldBeAt` 3

            it "breaks by rejecting" $ do
                let loop = While TRUE Reject
                evalStm loop `shouldReject` testConfig

            it "breaks by accepting" $ do
                let loop = While TRUE Accept
                evalStm loop `shouldAccept` testConfig

        context "evaluating function declarations" $ do
            it "adds functions to the environment" $ do
                let decl = Func "f" MoveRight
                let Inter (_, _, envf) = evalStm decl initialConfig
                envf "f" `shouldBe` (Just MoveRight)

        context "evaluating function calls" $ do
            it "performs the function" $ do
                let decl = Func "f" MoveRight
                    call = Call "f"
                    comp = Comp decl call
                evalStm comp initialConfig `shouldBeAt` 1

            it "evaluates a recursive function" $ do
                -- The statement used in the test is shown below. This function
                -- moves right until a '#' is encountered. When a '#' is found,
                -- the program accepts. If the end of the input is found
                -- (i.e. ' ') the program rejects.
                --
                --  func f {
                --      if read == # {
                --          accept
                --      } else if read == space {
                --          reject
                --      } else {
                --          right
                --          f
                --      }
                --  }
                --  f
                let b1            = Eq Read (Literal '#')
                    b2            = Eq Read (Literal ' ')
                    elseIfClauses = [(b2, Reject)]
                    elseClause    = Just (Comp MoveRight (Call "f"))
                    ifStm         = If b1 Accept elseIfClauses elseClause
                    funcDecl      = Func "f" ifStm
                    comp          = Comp funcDecl (Call "f")

                evalStm comp `shouldAccept` testConfig
                evalStm comp `shouldReject` nonTerminatedTestConfig

            it "evaluates nested functions, and removes the nested declarations once the scope is exited" $ do
                -- The statement used in the test is shown below.
                --
                --  func outer {
                --      write #
                --      func inner {
                --          right
                --      }
                --      inner
                --  }
                --  outer
                let innerBody               = MoveRight
                    innerDecl               = Func "inner" innerBody
                    outerBody               = Comp (Comp (Write '#') innerDecl) (Call "inner")
                    outerDecl               = Func "outer" outerBody
                    comp                    = Comp outerDecl (Call "outer")
                    Inter (tape, pos, envf) = evalStm comp testConfig

                tape 1 `shouldBe` '#'
                pos `shouldBe` 2
                envf "outer" `shouldBe` (Just outerBody)
                envf "inner" `shouldBe` Nothing

            it "evaluates nested functions where the name of the outer is overridden" $ do
                -- The statement used in the test is shown below.
                --
                --  func f {
                --      write #
                --      func f {
                --          right
                --      }
                --      f
                --  }
                --  f
                let innerBody               = MoveRight
                    innerDecl               = Func "f" innerBody
                    outerBody               = Comp (Comp (Write '#') innerDecl) (Call "f")
                    outerDecl               = Func "f" outerBody
                    comp                    = Comp outerDecl (Call "f")
                    Inter (tape, pos, envf) = evalStm comp testConfig

                tape 1 `shouldBe` '#'
                pos `shouldBe` 2
                envf "f" `shouldBe` (Just outerBody)
