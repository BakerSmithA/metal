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

-- A type that llows for special accept and reject states, as well an
-- intermediate states.
data ProgState a = Inter a
                 | HaltR
                 | HaltA

instance Functor ProgState where
    -- fmap :: (a -> b) -> State a -> State b
    fmap f (Inter x) = Inter (f x)
    fmap f (HaltR)   = HaltR
    fmap f (HaltA)   = HaltA

instance Applicative ProgState where
    -- pure :: a -> State a
    pure = Inter
    -- (<*>) :: State (a -> b) -> State a -> State b
    (Inter f) <*> s = fmap f s

instance Monad ProgState where
    -- (>>=) :: State a -> (a -> State b) -> State b
    (Inter x) >>= f = f x
    (HaltR)   >>= f = HaltR
    (HaltA)   >>= f = HaltA

type State = ProgState Config
