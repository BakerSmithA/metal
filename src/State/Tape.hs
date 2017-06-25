module State.Tape
( Pos
, Tape
, empty
, fromString
, getSym
, setSym
) where

import qualified Data.Map as Map
import Syntax.Tree

-- A position on the tape, i.e. the index of a cell.
type Pos = Integer

-- A representation of a tape. Although not explicitly modelled, the tape
-- streches only to the right, stopping at the zero position.
newtype Tape = Tape (Map.Map Pos TapeSymbol) deriving (Eq)

instance Show Tape where
    show = toString

-- The empty tape, where each position maps to the space symbol, i.e. ' '
empty :: Tape
empty = Tape Map.empty

-- A string representation of the tape, i.e. the cells of the tape in order.
toString :: Tape -> String
toString (Tape t) = map snd (Map.toAscList t)

-- A tape where the string `str` is placed at the start of the tape.
fromString :: String -> Tape
fromString str = foldr f empty enumerated where
    f (p, sym) = setSym p sym
    enumerated = zip [0..] str

-- Returns the tape symbol at the specified position.
getSym :: Pos -> Tape -> TapeSymbol
getSym pos (Tape t) = Map.findWithDefault ' ' pos t

-- Sets the tape symbol at the specified position.
setSym :: Pos -> TapeSymbol -> Tape -> Tape
setSym pos sym (Tape t) = Tape (Map.insert pos sym t)
