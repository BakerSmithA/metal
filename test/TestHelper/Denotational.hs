module TestHelper.Denotational where

import State.Tape
import Syntax.Tree
import State.App
import State.Config as Config
import State.Error
import State.Machine
import State.Tape as T
import Semantics.Bexp
import Semantics.DerivedSymbol
import Semantics.Stm
import qualified Test.Hspec as H
import Test.HUnit.Lang

type AppResult a = Either RuntimeError (Machine (a, [String]))

evalWith :: (a -> Config -> App b) -> a -> Config -> AppResult b
evalWith f x config = evalApp (f x config)

-- Runs `derivedSymbolVal` with `sym` in the given config and environment.
evalDerivedSymbol :: DerivedSymbol -> Config -> AppResult TapeSymbol
evalDerivedSymbol = evalWith derivedSymbolVal

-- Runs `bexpVal` with `b` in the given config and environment.
evalBexp :: Bexp -> Config -> AppResult Bool
evalBexp = evalWith bexpVal

evalSemantics :: Stm -> Config -> AppResult Config
evalSemantics = evalWith evalStm

-- Asserts that the app threw the given runtime error.
shouldThrow :: (Show a) => AppResult a -> RuntimeError -> H.Expectation
shouldThrow result expected = either (`H.shouldBe` expected) failure result where
    failure c = assertFailure ("Expected error, got: " ++ (show c))

-- Asserts that when the semantics have finished being evaulated, the resulting
-- machine satisfies the given predicate.
shouldSatisfyMachine :: (Show a) => AppResult a -> (Machine (a, [String]) -> Bool) -> H.Expectation
shouldSatisfyMachine result predicate = either failure match result where
    failure err = assertFailure ("Expected machine, got error: " ++ (show err))
    match m = m `H.shouldSatisfy` predicate

-- Asserts that the app didn't throw an error, but instead resulted in the
-- given machine (ignoring any output).
shouldBeMachine :: (Eq a, Show a) => AppResult a -> Machine a -> H.Expectation
shouldBeMachine result expected = result `shouldSatisfyMachine` predicate where
    predicate actual = fmap fst actual == expected

-- Asserts that the app outputted the given list of strings.
shouldOutput :: (Show a) => AppResult a -> [String] -> H.Expectation
shouldOutput result expected = result `shouldSatisfyMachine` predicate where
    predicate actual = fmap snd actual == return expected

-- Asserts that the result of running the app satisfies the given predicate.
shouldSatisfy :: (Show a) => AppResult a -> (a -> Bool) -> H.Expectation
shouldSatisfy result predicate = result `shouldSatisfyMachine` predicate' where
    predicate' = machine False False (predicate . fst)

-- Asserts that the app contains the given value.
shouldReturn :: (Eq a, Show a) => AppResult a -> a -> H.Expectation
shouldReturn result = (shouldBeMachine result) . return

-- Asserts that the variable environment contains the given variable name and
-- corresponding value.
shouldContainVar :: AppResult Config -> VarName -> TapeSymbol -> H.Expectation
shouldContainVar result name sym = shouldSatisfy result predicate where
    predicate config = lookupVar name config == Just sym

-- Asserts that the function environment contains the given function name and
-- corresponding arguments and body.
shouldContainFunc :: AppResult Config -> FuncName -> FuncDeclArgs -> Stm -> H.Expectation
shouldContainFunc result name args body = shouldSatisfy result predicate where
    predicate config = lookupFunc name config == Just (args, body)

-- Asserts that the read-write head is at the given position.
shouldBeAt :: AppResult Config -> Pos -> H.Expectation
shouldBeAt result expected = shouldSatisfy result ((== expected) . pos)

-- Asserts that the tape contains the given symbols at is start.
shouldRead :: AppResult Config -> [TapeSymbol] -> H.Expectation
shouldRead result expected = shouldSatisfy result ((== T.fromString expected) . tape)

-- Asserts that the app accepted.
shouldAccept :: (Eq a, Show a) => AppResult a -> H.Expectation
shouldAccept result = result `shouldBeMachine` HaltA

-- Asserts that app rejected.
shouldReject :: (Eq a, Show a) => AppResult a -> H.Expectation
shouldReject result = result `shouldBeMachine` HaltR
