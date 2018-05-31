module TestHelper.Config where

import State.Config
import State.Tape as Tape
import Data.Map as Map (assocs)
import Data.Maybe

-- Expectes there to only be one tape defined. If so, that tape will be
-- modified. Otherwise an error will be thrown.
modifySingleTape :: (Tape -> Tape) -> Config -> Config
modifySingleTape f c = fromJust $ modifyTape tapeName f c where
    tapeName = getFirstTape tapes
    tapes = filter isTapeRef (Map.assocs (vars c))

    getFirstTape [(name, _)] = name
    getFirstTape _           = error "Expected only 1 tape"

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
