module Syntax.ParseState where

-- Keeps track of used variable and function names to avoid invalid programs.
data ParseState = ParseState {
    -- Variables named in the scope above, these can be overwritten.
    aboveScope :: [String],
    -- Variables in the current scope, these **cannot** be overwritten.
    used :: [String]
}

-- Contains no variables.
empty :: ParseState
empty = ParseState [] []

-- State containing the list of used variable names at the current scope.
fromList :: [String] -> ParseState
fromList used = ParseState [] used

-- Adds a variable name to the current scope.
putVar :: String -> ParseState -> ParseState
putVar varId (ParseState above used) = ParseState above (varId:used)

-- Moves any used names into the scope above.
descendScope :: ParseState -> ParseState
descendScope (ParseState above used) = ParseState (above ++ used) []

-- Returns whether a variable name can be used to declare a new variable, i.e.
-- if the name is in use at this scope.
isTaken :: String -> ParseState -> Bool
isTaken varId (ParseState _ used) = varId `elem` used

-- Returns whether a variable name can be used, i.e. if the variable has been
-- declared in this scope or the scope above.
canRef :: String -> ParseState -> Bool
canRef varId (ParseState above used) = varId `elem` (above ++ used)
