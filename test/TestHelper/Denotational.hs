module TestHelper.Denotational where

import State.Tape
import Syntax.Tree
import State.Config as Config
import State.Error
import State.Machine
import State.App
import Semantics.Bexp
import Semantics.DerivedSymbol
import Semantics.Stm
import Test.Hspec hiding (shouldContain, shouldThrow)
import Test.HUnit.Lang
import TestHelper.Tape

type AppResult a = Either RuntimeError (Machine a)

-- Runs `derivedSymbolVal` with `sym` in the given config and environment.
evalDerivedSymbol :: DerivedSymbol -> Config -> AppResult TapeSymbol
evalDerivedSymbol sym config = runApp' (derivedSymbolVal sym (return config))

-- Runs `bexpVal` with `b` in the given config and environment.
evalBexp :: Bexp -> Config -> AppResult Bool
evalBexp b config = runApp' (bexpVal b (return config))

-- Runs `s` in config with the empty environment.
evalSemantics :: Stm -> Config -> AppResult Config
evalSemantics s config = runApp' (evalStm s (return config))

-- Asserts that a runtime error was thrown.
shouldThrow :: (Show a) => AppResult a -> RuntimeError -> Expectation
shouldThrow r expected = either handleErr success r where
    handleErr err = err `shouldBe` expected
    success mach  = assertFailure ("Expected err, got machine: " ++ (show mach))

-- Asserts that when the semantics have finished being evaulated, the resulting
-- machine satisfies the given predicate.
machShouldSatify :: (Eq a, Show a) => AppResult a -> (Machine a -> Bool) -> Expectation
machShouldSatify r predicate = either handleErr success r where
    handleErr err = assertFailure ("Expected machine, got error: " ++ (show err))
    success mach  = mach `shouldSatisfy` predicate

-- Asserts that when the semantics have finished being evaulated, the value
-- wrapped in the machine satisfies the predicate.
shouldSatify :: (Eq a, Show a) => AppResult a -> (a -> Bool) -> Expectation
shouldSatify r predicate = machShouldSatify r f where
    f = machine False False predicate

-- Asserts that when the semantics have finished being evaulated, the machine
-- contains the given value.
shouldContain :: (Eq a, Show a) => AppResult a -> a -> Expectation
shouldContain r sym = shouldSatify r (== sym)

-- Asserts that the variable environment contains the given value for the
-- variable name.
shouldContainVar :: AppResult Config -> VarName -> TapeSymbol -> Expectation
shouldContainVar r name sym = shouldSatify r predicate where
    predicate config = lookupVar name config == Just sym

-- Asserts that the function environment contains the given function body for
-- the function name.
shouldContainFunc :: AppResult Config -> FuncName -> FuncDeclArgs -> Stm -> Expectation
shouldContainFunc r name args body = shouldSatify r predicate where
    predicate config = lookupFunc name config == Just (args, body)

-- Asserts that when the semantics have finished being evauluated, the position
-- of the read-write head is in the given position.
shouldBeAt :: AppResult Config -> Pos -> Expectation
shouldBeAt r p = shouldSatify r predicate where
    predicate c = pos c == p

-- Asserts that the tape has the string `str` at the start of the tape.
shouldRead :: AppResult Config -> [TapeSymbol] -> Expectation
shouldRead r syms = shouldSatify r predicate where
    predicate c = tapeShouldRead (tape c) syms

-- Asserts that the machine halted in the accepting state.
shouldAccept :: AppResult Config -> Expectation
shouldAccept r = machShouldSatify r (== HaltA)

-- Asserts that the machine halted in the rejecting state.
shouldReject :: AppResult Config -> Expectation
shouldReject r = machShouldSatify r (== HaltR)
