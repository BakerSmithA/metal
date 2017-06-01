module Semantics.State where

import Syntax.Tree

-- A position on a tape, i.e. the index of the cell.
type Pos = Integer

-- Although not explicitly modelled, the tape extendeds only to the right,
-- stopping at the 0 index.
type Tape = Pos -> TapeSymbol

-- An initial tape, with an entirely empty tape, i.e. every cell maps to the
-- space character.
initialTape :: Tape
initialTape p = ' '

-- An environment for variables, i.e. a mapping from variable names to the
-- tape symbol associated with the variable.
type EnvV = VarName -> Maybe TapeSymbol

-- An initial variable environment, where every variable name maps to no value.
initialEnvV :: EnvV
initialEnvV vName = Nothing

-- An environment for functions, i.e. a mapping from function names to the body
-- of that function.
type EnvF = FuncName -> Maybe Stm

-- An initial function environment, where every function name maps to no body.
initialEnvF :: EnvF
initialEnvF fName = Nothing

-- Returns the body of a function according to the name of a function.
-- An runtime error is produced if the function has not been defined.
funcBody :: FuncName -> EnvF -> Stm
funcBody fName envf = case envf fName of
    Just body -> body
    Nothing   -> error ("No function named " ++ fName)

-- Here we differ slightly from the denotational semantics of the
-- specification by ommitting the output (e.g. using print) in the state.
type Config = (Tape, Pos, EnvV, EnvF)

-- An initial configuration in which the tape is empty, the read/write head is
-- in the left-most zeroed position, and the function environment is empty.
initialConfig :: Config
initialConfig = (initialTape, 0, initialEnvV, initialEnvF)

-- A type that allows for special accept and reject states, as well as
-- intermediate states.
data ProgState a = Inter a
                 | HaltR
                 | HaltA
                 deriving (Eq)

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

-- An initial state in which the tape is empty, the read/write head is
-- in the left-most zeroed position, and the function environment is empty.
initialState :: State
initialState  = Inter initialConfig
