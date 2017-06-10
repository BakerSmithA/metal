module State.Config where

import State.MachineState

-- A configuration of a Turing machine.
data Config = Config {
    pos :: Integer -- The position the read-write head.
}

instance Show Config where
    show (Config pos) = show pos

-- A configuration in which the read-write head is in the zeroed position.
initial :: Config
initial = Config 0

-- Moves the read-write head one cell to the left, provided the head is not in
-- the zeroed position, in this case no action occurs.
left :: Config -> Config
left c = c { pos = max p' 0 } where
    p' = pos c - 1

-- Moves the read-write head one cell to the right.
right :: Config -> Config
right c = c { pos = (pos c) + 1 }

-- A representation of Turing machine.
type Machine = MachineState Config
