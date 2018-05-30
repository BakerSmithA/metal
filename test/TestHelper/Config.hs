module TestHelper.Config where

import State.Config
import State.Tape as Tape
import Syntax.Tree
import Data.Map as Map (assocs)

modifySingleTape :: (Tape -> Tape) -> Config -> Config
modifySingleTape f c@(Config ts _ _) = putTape name (f tape) c where
    (name, tape) = getFirstTape (Map.assocs ts)
    getFirstTape [tapeData] = tapeData
    getFirstTape _          = error "Expected only 1 tape"

-- Expects the config to only contain one tape. If so, the head will be moved
-- to the left.
left :: Config -> Config
left = modifySingleTape Tape.left

-- Expects the config to only contain one tape. If so, the head will be moved
-- to the left.
right :: Config -> Config
right = modifySingleTape Tape.right
