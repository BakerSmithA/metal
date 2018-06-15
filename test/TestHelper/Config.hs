module TestHelper.Config where

import Syntax.Tree
import State.Config
import State.Tape as Tape
import Data.Map as Map (assocs)
import Data.Maybe

import Debug.Trace

-- Looks up the address of the tape and then deferences it, therefore changes
-- to the returned tape will not be reflected in the environment.
getTapeCpy :: VarPath -> Config -> Maybe Tape
getTapeCpy name c = do
    addr <- getPtr name c
    (TapeRef t) <- derefPtr addr c
    return t

-- Looks up the address of the object and then deferences it, therefore changes
-- to the returned tape will not be reflected in the environment.
getObjCpy :: VarPath -> Config -> Maybe Object
getObjCpy name c = do
    addr <- getPtr name c
    (ObjRef t) <- derefPtr addr c
    return t

-- Convenience method for modifying the a tape with the given name.
modifyNamedTape :: VarPath -> (Tape -> Tape) -> Config -> Maybe Config
modifyNamedTape name f c = modifyTape addr f c where
    addr = fromJust (getPtr name c)

-- Expectes there to only be one object defined, i.e. a tape If so, that tape
-- will be modified. Otherwise an error will be thrown.
modifySingleTape :: (Tape -> Tape) -> Config -> Config
modifySingleTape f c = fromJust $ modifyNamedTape [ptrName] f c where
    ptrName = getFirstPtr ptrs
    ptrs = filter isPtr (Map.assocs (vars c))

    getFirstPtr [(name, _)] = name
    getFirstPtr _           = error "Expected only 1 object"

    isPtr (_, (Ptr _)) = True
    isPtr _ = False

-- Expects the config to only contain one tape. If so, the head will be moved
-- to the left.
left :: Config -> Config
left = modifySingleTape Tape.left

-- Expects the config to only contain one tape. If so, the head will be moved
-- to the left.
right :: Config -> Config
right = modifySingleTape Tape.right
