module Syntax.Env where

import Syntax.Tree (Identifier)

-- Keeps track of declarations at this scope, and the scope above.
data Env = Env {
    -- Declarations in the scope above. These can be overwritten.
    aboveScope :: [Identifier],
    -- Declarations in the current scope. These cannot be overwritten.
    used :: [Identifier]
}

empty :: Env
empty = Env [] []

-- State containing the list of used identifiers at the current scope.
fromList :: [Identifier] -> Env
fromList used = Env [] used

-- Adds a variable name to the current scope.
put :: Identifier -> Env -> Env
put identifier (Env above used) = Env above (identifier:used)

-- Moves any used names into the scope above.
descendScope :: Env -> Env
descendScope (Env above used) = Env (above ++ used) []

-- Returns whether a identifier can be used to declare a new variable/functions,
-- i.e. if the name is in use at this scope.
isTaken :: Identifier -> Env -> Bool
isTaken identifier (Env _ used) = identifier `elem` used

-- Returns whether a identifier can be used, i.e. if the identifier has been
-- declared in this scope or the scope above.
canRef :: Identifier -> Env -> Bool
canRef identifier (Env above used) = identifier `elem` (above ++ used)
