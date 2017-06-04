module Semantics.State where

import Syntax.Tree

-- Updates the value of `f x` to be `r`.
update :: (Eq a) => a -> b -> (a -> b) -> (a -> b)
update x r f x' = if x' == x then r else f x'

-- Updates the values of `f x_i` for all i, to be r_i.
updateMany :: (Eq a) => [(a, b)] -> (a -> b) -> (a -> b)
updateMany xs f = foldr (\(x, r) acc -> update x r acc) f xs

-- A position on a tape, i.e. the index of the cell.
type Pos = Integer

-- Although not explicitly modelled, the tape extendeds only to the right,
-- stopping at the 0 index.
type Tape = Pos -> TapeSymbol

-- An initial tape, with an entirely empty tape, i.e. every cell maps to the
-- space character.
initialTape :: Tape
initialTape p = ' '

-- Places `str` at the start of the tape.
placeOnTape :: String -> Tape -> Tape
placeOnTape str = updateMany (zip [0..] str)

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

-- A configuration in which the tape contains the string `str`, the read/write
-- head is in the left-most zeroed position, and the function environment is
-- empty.
configFromTapeStr :: String -> Config
configFromTapeStr str = (tape, 0, initialEnvV, initialEnvF) where
    tape = placeOnTape str initialTape

-- An initial configuration in which the tape is empty, the read/write head is
-- in the left-most zeroed position, and the function environment is empty.
initialConfig :: Config
initialConfig = configFromTapeStr ""

-- A type that allows for special accept and reject states, as well as
-- intermediate states.
data ProgState a = Inter a
                 | HaltR
                 | HaltA
                 deriving (Eq)

instance Functor ProgState where
    -- fmap :: (a -> b) -> ProgState a -> ProgState b
    fmap f (Inter x) = Inter (f x)
    fmap f (HaltR)   = HaltR
    fmap f (HaltA)   = HaltA

instance Applicative ProgState where
    -- pure :: a -> ProgState a
    pure = Inter
    -- (<*>) :: ProgState (a -> b) -> ProgState a -> ProgState b
    (Inter f) <*> s = fmap f s

instance Monad ProgState where
    -- (>>=) :: ProgState a -> (a -> ProgState b) -> ProgState b
    (Inter x) >>= f = f x
    (HaltR)   >>= f = HaltR
    (HaltA)   >>= f = HaltA

type State = ProgState Config
