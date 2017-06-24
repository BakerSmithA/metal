module TestHelper.Tape where

import Data.Map
import State.Tape
import Syntax.Tree

-- Returns whether the tape should contain the string `syms` at its start.
tapeShouldRead :: Tape -> [TapeSymbol] -> Bool
tapeShouldRead tape syms = tape == expected where
    expected = fromList (zip [0..] syms)
