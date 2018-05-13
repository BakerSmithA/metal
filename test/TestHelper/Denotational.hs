module TestHelper.Denotational where

import State.Tape as T
import Syntax.Tree
import State.App
import State.Config as Config
import State.Machine
import State.Output
import Semantics.Bexp
import Semantics.DerivedSymbol
import Semantics.Stm
import Semantics.Program
import qualified Test.Hspec as H
import TestHelper.Output

type AppResult m a = m (Machine a)

evalWith :: (a -> Config -> App m b) -> a -> Config -> AppResult m b
evalWith f x config = evalApp (f x config)

-- Runs `derivedSymbolVal` with `sym` in the given config and environment.
evalDerivedSymbol :: (Monad m) => DerivedSymbol -> Config -> AppResult m TapeSymbol
evalDerivedSymbol = evalWith derivedSymbolVal

-- Runs `bexpVal` with `b` in the given config and environment.
evalBexp :: (Monad m) => Bexp -> Config -> AppResult m Bool
evalBexp = evalWith bexpVal

evalSemantics :: (MonadOutput m) => Stm -> Config -> AppResult m Config
evalSemantics = evalWith evalStm

evalProgram :: (MonadOutput m) => Program -> Config -> AppResult m Config
evalProgram = evalWith evalProg

-- Asserts that when the semantics have finished being evaulated, the value
-- wrapped in the machine satisfies the predicate.
shouldSatisfy :: (Eq a, Show a) => IO (Machine a) -> (a -> Bool) -> H.Expectation
shouldSatisfy result predicate = result >>= (`H.shouldSatisfy` f) where
    f = machine False False predicate

-- Asserts that when the semantics have finished being evaulated, the machine
-- contains the given value.
shouldReturn :: (Eq a, Show a) => IO (Machine a) -> a -> H.Expectation
shouldReturn result expected = result `H.shouldReturn` (return expected)

-- Asserts that the variable environment contains the given value for the
-- variable name.
shouldReturnVar :: IO (Machine Config) -> VarName -> TapeSymbol -> H.Expectation
shouldReturnVar r name sym = shouldSatisfy r predicate where
    predicate config = lookupVar name config == Just sym

-- Asserts that the function environment contains the given function body for
-- the function name.
shouldReturnFunc :: IO (Machine Config) -> FuncName -> FuncDeclArgs -> Stm -> H.Expectation
shouldReturnFunc r name args body = shouldSatisfy r predicate where
    predicate config = lookupFunc name config == Just (args, body)

-- Asserts that when the semantics have finished being evauluated, the position
-- of the read-write head is in the given position.
shouldBeAt :: IO (Machine Config) -> Pos -> H.Expectation
shouldBeAt r p = shouldSatisfy r predicate where
    predicate c = pos c == p

-- Asserts that the tape has the string `str` at the start of the tape.
shouldRead :: IO (Machine Config) -> [TapeSymbol] -> H.Expectation
shouldRead r syms = shouldSatisfy r predicate where
    predicate c = tape c == T.fromString syms

-- Asserts that the machine halted in the accepting state.
shouldAccept :: IO (Machine Config) -> H.Expectation
shouldAccept r = r >>= (`H.shouldBe` HaltA)

-- Asserts that the machine halted in the rejecting state.
shouldReject :: IO (Machine Config) -> H.Expectation
shouldReject r = r >>= (`H.shouldBe` HaltR)

-- Asserts that the machine outputted the given strings.
shouldOutput :: TestM (Machine Config) -> [String] -> H.Expectation
shouldOutput r expected = logTestM r `H.shouldBe` expected
