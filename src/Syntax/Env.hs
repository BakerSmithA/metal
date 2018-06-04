module Syntax.Env where

import Syntax.Tree (Identifier)

-- Keeps track of declarations at this scope, and the scope above.
data Env = Env {
    -- Declarations in the scope above. These can be overwritten.
    aboveScope :: [Identifier],
    -- Declarations in the current scope. These cannot be overwritten.
    used :: [Identifier]
}

-- Contains no variables.
empty :: Env
empty = Env [] []

-- State containing the list of used variable names at the current scope.
fromList :: [Identifier] -> Env
fromList used = Env [] used

-- Adds a variable name to the current scope.
putVar :: Identifier -> Env -> Env
putVar varId (Env above used) = Env above (varId:used)

-- Moves any used names into the scope above.
descendScope :: Env -> Env
descendScope (Env above used) = Env (above ++ used) []

-- Returns whether a variable name can be used to declare a new variable, i.e.
-- if the name is in use at this scope.
isTaken :: Identifier -> Env -> Bool
isTaken varId (Env _ used) = varId `elem` used

-- Returns whether a variable name can be used, i.e. if the variable has been
-- declared in this scope or the scope above.
canRef :: Identifier -> Env -> Bool
canRef varId (Env above used) = varId `elem` (above ++ used)
