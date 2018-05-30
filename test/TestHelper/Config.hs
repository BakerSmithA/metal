module TestHelper.Config where

import State.Config
import State.Tape as Tape
import Syntax.Tree
import Data.Map as Map (assocs)

-- Expectes there to only be one tape defined. If so, that tape will be
-- modified. Otherwise an error will be thrown.
modifySingleTape :: (Tape -> Tape) -> Config -> Config
modifySingleTape f c@(Config vs _) = putTape name (f tape) c where
    (name, (TapeRef tape)) = getFirstTape tapes
    tapes = filter isTapeRef (Map.assocs vs)

    getFirstTape [tapeData] = tapeData
    getFirstTape _          = error "Expected only 1 tape"

    isTapeRef (_, (TapeRef _)) = True
    isTapeRef _ = False

-- Expects the config to only contain one tape. If so, the head will be moved
-- to the left.
left :: Config -> Config
left = modifySingleTape Tape.left

-- Expects the config to only contain one tape. If so, the head will be moved
-- to the left.
right :: Config -> Config
right = modifySingleTape Tape.right
