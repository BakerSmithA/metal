module State.Config where

import State.Tape as Tape
import Syntax.Tree

-- A configuration of a Turing machine.
data Config = Config {
    pos  :: Pos  -- The position the read-write head.
  , tape :: Tape -- The tape of the Turing machine.
} deriving (Eq)

instance Show Config where
    -- show :: Config -> String
    show c = show (pos c)

-- A configuration in which the read-writ head is in the zeroed position, and
-- the tape is empty.
initial :: Config
initial = Config 0 Tape.empty

-- A configuration in which the read-write head is in the zeroed position, and
-- `str` is at the start of the tape.
fromString :: String -> Config
fromString str = Config 0 (Tape.fromString str)

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
getCurr (Config p t) = getSym p t

-- Writes a symbol at the current position of the read-write head.
setCurr :: TapeSymbol -> Config -> Config
setCurr sym (Config p t) = Config p t' where
    t' = setSym p sym t
