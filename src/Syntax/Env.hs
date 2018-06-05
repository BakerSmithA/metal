module Syntax.Env where

import Syntax.Tree (Identifier)
import Data.Map as Map

-- Keeps track of declarations at this scope, and the scope above.
-- And the type of identifiers in the environment.
data Env a = Env {
    -- Declarations in the scope above. These can be overwritten.
    aboveScope :: Map Identifier a,
    -- Declarations in the current scope. These cannot be overwritten.
    used :: Map Identifier a
}

-- Returns the combination of both scopes to make for easier searching.
combinedScopes :: Env a -> Map Identifier a
combinedScopes (Env above used) = Map.union above used

empty :: Env a
empty = Env Map.empty Map.empty

-- State containing the list of used identifiers at the current scope.
fromList :: [(Identifier, a)] -> Env a
fromList usedIds = Env Map.empty (Map.fromList usedIds)

-- Adds a variable name to the current scope.
put :: Identifier -> a -> Env a -> Env a
put newId idType (Env above used) = Env above used' where
    used' = Map.insert newId idType used

-- Moves any used names into the scope above.
descendScope :: Env a -> Env a
descendScope env = Env (combinedScopes env) Map.empty

-- Returns whether a identifier can be used to declare a new variable/functions,
-- i.e. if the name is **not** in use at this scope.
isAvailable :: Identifier -> Env a -> Bool
isAvailable i (Env _ used) = i `Map.notMember` used

-- Returns whether a identifier can be used, i.e. if the identifier has been
-- declared in this scope or the scope above.
canRef :: Identifier -> Env a -> Bool
canRef i env = i `Map.member` (combinedScopes env)

-- Returns whether the expected type matches the type of the identifier, or
-- returns False if the identifier does not exist.
hasMatchingType :: (Eq a) => a -> Identifier -> Env a -> Bool
hasMatchingType expectedType i env = Map.lookup i (combinedScopes env) == Just expectedType
