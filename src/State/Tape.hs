module State.Tape where

import qualified Data.Map as Map
import Syntax.Tree (TapeSymbol)

-- A position on the tape, i.e. the index of a cell.
type Pos = Integer

-- A representation of a tape. Although not explicitly modelled, the tape
-- streches only to the right, stopping at the zero position.
data Tape = Tape {
    pos :: Pos
  , contents :: (Map.Map Pos TapeSymbol)
} deriving (Eq)

instance Show Tape where
    show (Tape p t) = "p=" ++ (show p) ++ ", contents=" ++ map snd (Map.toAscList t)

-- The empty tape, where each position maps to the space symbol, i.e. ' '
empty :: Tape
empty = Tape 0 Map.empty

-- A tape where the string `str` is placed at the start of the tape.
fromString :: String -> Tape
fromString str = Tape 0 cs where
    cs = Map.fromList $ zip [0..] str

-- Returns the tape symbol at the specified position.
getSym :: Tape -> TapeSymbol
getSym (Tape p t) = Map.findWithDefault ' ' p t

-- Sets the tape symbol at the specified position.
setSym :: TapeSymbol -> Tape -> Tape
setSym sym (Tape p t) = Tape p (Map.insert p sym t)

-- Moves the head of the tape to the left, or performs no action if the head
-- already zeroed.
left :: Tape -> Tape
left (Tape 0 c) = Tape 0 c
left (Tape p c) = Tape (p-1) c

-- Moves the head of the tape to the right.
right :: Tape -> Tape
right (Tape p c) = Tape (p+1) c
