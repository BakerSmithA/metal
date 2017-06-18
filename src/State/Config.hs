module State.Config
( Config
, left
, right
, getCurr
, setCurr
) where

import Syntax.Tree
import State.Tape
import State.Machine

-- A configuration of a Turing machine.
data Config = Config {
    pos  :: Pos  -- The position the read-write head.
  , tape :: Tape -- The tape of the Turing machine.
}

instance Show Config where
    -- show :: Config -> String
    show (Config pos tape) = show pos

-- A configuration in which the read-write head is in the zeroed position.
initial :: Config
initial = Config 0 empty

-- Moves the read-write head one cell to the left, provided the head is not in
-- the zeroed position, in this case no action occurs.
left :: Config -> Config
left c = c { pos = max p' 0 } where
    p' = pos c - 1

-- Moves the read-write head one cell to the right.
right :: Config -> Config
right c = c { pos = (pos c) + 1 }

-- Reads the symbol under the read-write head.
getCurr :: Config -> TapeSymbol
getCurr (Config pos tape) = getSym pos tape

-- Writes a symbol at the current position of the read-write head.
setCurr :: TapeSymbol -> Config -> Config
setCurr sym (Config pos tape) = Config pos tape' where
    tape' = setSym pos sym tape
