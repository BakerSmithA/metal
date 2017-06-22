module TestHelper where

import Data.Map
import Control.Monad
import State.Tape
import Syntax.Tree
import Test.Hspec
import Test.HUnit.Lang

-- Asserts that the tape should contain the string `str` at its start.
tapeShouldRead :: Tape -> [TapeSymbol] -> Bool
tapeShouldRead tape syms = tape == expected where
    expected = fromList (zip [0..] syms)
