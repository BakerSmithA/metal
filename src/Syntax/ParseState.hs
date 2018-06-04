module Syntax.ParseState where

import Syntax.Env as E

-- Keeps track of used variable and function names to avoid invalid programs.
data ParseState = ParseState {
    varEnv :: Env,
    funcEnv :: Env
}

-- A parse state containing no variables or functions.
empty :: ParseState
empty = ParseState E.empty E.empty

-- A parse state containing variables with the given names.
fromVarList :: [String] -> ParseState
fromVarList vars = ParseState (E.fromList vars) E.empty

-- Applies f to both the variable and function environments.
modifyEnvs :: (Env -> Env) -> ParseState -> ParseState
modifyEnvs f (ParseState ve fe) = ParseState (f ve) (f fe)

-- Applies f to the variable environment only.
modifyVarEnv :: (Env -> Env) -> ParseState -> ParseState
modifyVarEnv f (ParseState ve fe) = ParseState (f ve) fe

-- Applies f to the function environment only.
modifyFuncEnv :: (Env -> Env) -> ParseState -> ParseState
modifyFuncEnv f (ParseState ve fe) = ParseState ve (f fe)
