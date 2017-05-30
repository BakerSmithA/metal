module Semantics.State where

import Syntax.Tree

-- A position on a tape, i.e. the index of the cell.
type Pos = Integer

-- Although not explicitly modelled, the tape extendeds only to the right,
-- stopping at the 0 index.
type Tape = Pos -> TapeSymbol

-- An environment for functions, i.e. a mapping from function names to the body
-- of that function.
type EnvF = FuncName -> Stm

-- Here we slightly from the denotational semantics specified in the
-- specification by ommitting the output in the state.
type Config = (Tape, Pos, EnvF)

-- `ProgState` allows for special accept and reject states, as well an
-- intermediate states.
data ProgState = Inter Config
               | HaltR
               | HaltA
