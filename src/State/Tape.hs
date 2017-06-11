module State.Tape where

import Syntax.Tree
import Control.Monad.Reader

-- A position on the tape, i.e. the index of a cell.
type Pos = Integer

-- A representation of a tape. Although not explicitly modelled, the tape
-- streches only to the right, stopping at the zero position.
type Tape = Pos -> TapeSymbol

-- The empty tape, where each position maps to the space symbol, i.e. ' '
empty :: Tape
empty = const ' '

-- Returns the tape symbol at the specified position.
getSym :: Pos -> Tape -> TapeSymbol
getSym pos tape = tape pos

-- Sets the tape symbol at the specified position.
setSym :: Pos -> TapeSymbol -> Tape -> Tape
setSym pos sym tape pos' = if pos' == pos
                              then sym
                              else tape pos'
