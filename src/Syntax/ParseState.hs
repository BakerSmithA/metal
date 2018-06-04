module Syntax.ParseState where

import Syntax.Env as E
import Syntax.Tree (VarName, FuncName)

-- Keeps track of used variable and function names to avoid invalid programs.
data ParseState = ParseState {
    varEnv :: Env,
    funcEnv :: Env
}

-- A parse state containing no variables or functions.
empty :: ParseState
empty = ParseState E.empty E.empty

-- A parse state containing variables and functions with the given names.
fromLists :: [VarName] -> [FuncName] -> ParseState
fromLists vars funcs = ParseState (E.fromList vars) (E.fromList funcs)

-- A parse state containing only variables with the given names.
fromVarList :: [VarName] -> ParseState
fromVarList vars = fromLists vars []

-- A parse state containing only functions with the given names.
fromFuncList :: [VarName] -> ParseState
fromFuncList funcs = fromLists [] funcs

-- Applies f to both the variable and function environments.
modifyEnvs :: (Env -> Env) -> ParseState -> ParseState
modifyEnvs f (ParseState ve fe) = ParseState (f ve) (f fe)

-- Applies f to the variable environment only.
modifyVarEnv :: (Env -> Env) -> ParseState -> ParseState
modifyVarEnv f (ParseState ve fe) = ParseState (f ve) fe

-- Applies f to the function environment only.
modifyFuncEnv :: (Env -> Env) -> ParseState -> ParseState
modifyFuncEnv f (ParseState ve fe) = ParseState ve (f fe)
