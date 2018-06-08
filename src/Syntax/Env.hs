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

instance (Show a) => Show (Env a) where
    show (Env a u) = "above: " ++ (show a) ++ ", used: " ++ (show u)

-- Returns the combination of both scopes to make for easier searching.
combinedScopes :: Env a -> Map Identifier a
-- used before above so conficts use the inner-most definiton.
combinedScopes (Env above used) = Map.union used above

empty :: Env a
empty = Env Map.empty Map.empty

-- State containing the list of used identifiers at the current scope.
fromList :: [(Identifier, a)] -> Env a
fromList usedIds = Env Map.empty (Map.fromList usedIds)

-- Adds a variable name to the current scope.
put :: Identifier -> a -> Env a -> Env a
put newId idType (Env above used) = Env above used' where
    used' = Map.insert newId idType used

-- Returns whether a identifier can be used, i.e. if the identifier has been
-- declared in this scope or the scope above.
get :: Identifier -> Env a -> Maybe a
get i env = i `Map.lookup` (combinedScopes env)

-- Retrive the identifier from the environment and modify it. If the identifier
-- does not exist then the supplied env is returned.
modify :: Identifier -> (a -> a) -> Env a -> Env a
modify i f env = case get i env of
    Nothing -> env
    Just x  -> put i (f x) env

-- Moves any used names into the scope above.
descendScope :: Env a -> Env a
descendScope env = Env (combinedScopes env) Map.empty

-- Returns whether a identifier has already been used to declare a variable/function.
-- i.e. if the name is in use at this scope.
isTaken :: Identifier -> Env a -> Bool
isTaken i (Env _ used) = i `Map.member` used
