module State.Config where

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
leftC :: Config -> Config
leftC c = c { pos = max p' 0 } where
    p' = pos c - 1

-- Moves the read-write head one cell to the right.
rightC :: Config -> Config
rightC c = c { pos = (pos c) + 1 }

-- Reads the symbol under the read-write head.
getCurrC :: Config -> TapeSymbol
getCurrC (Config pos tape) = getSym pos tape

-- Writes a symbol at the current position of the read-write head.
setCurrC :: TapeSymbol -> Config -> Config
setCurrC sym (Config pos tape) = Config pos tape' where
    tape' = setSym pos sym tape

-- A representation of Turing machine.
type MachineConfig = Machine Config

-- Moves the read-write head of the machine left, provided the machine is not
-- halted.
left :: MachineConfig -> MachineConfig
left = fmap leftC

-- Moves the read-write head of the machine right, provided the machine is not
-- halted.
right :: MachineConfig -> MachineConfig
right = fmap rightC

-- Retrieves the symbol under the read-write head.
getCurr :: MachineConfig -> Machine TapeSymbol
getCurr = fmap getCurrC

-- Writes a symbol at the current position of the read-write head.
setCurr :: TapeSymbol -> MachineConfig -> MachineConfig
setCurr sym = fmap (setCurrC sym)
