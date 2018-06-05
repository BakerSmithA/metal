module Syntax.ParseState where

import Syntax.Env as E
import Syntax.Tree (VarName, FuncName, DataType)

-- Stores variable identifiers and their associated types.
type VarEnv = Env DataType
-- Stores function identifiers and their associated argument types.
type FuncEnv = Env [DataType]

-- Keeps track of used variable and function names to avoid invalid programs.
data ParseState = ParseState {
    varEnv :: VarEnv,
    funcEnv :: FuncEnv
}

-- A parse state containing no variables or functions.
empty :: ParseState
empty = ParseState E.empty E.empty

-- A parse state containing variables and functions with the given names.
fromLists :: [(VarName, DataType)] -> [(FuncName, [DataType])] -> ParseState
fromLists vars funcs = ParseState (E.fromList vars) (E.fromList funcs)

-- A parse state containing only variables with the given names.
fromVarList :: [(VarName, DataType)] -> ParseState
fromVarList vars = fromLists vars []

-- A parse state containing only functions with the given names.
fromFuncList :: [(FuncName, [DataType])] -> ParseState
fromFuncList funcs = fromLists [] funcs

-- Applies f to the variable environment only.
modifyVarEnv :: (VarEnv -> VarEnv) -> ParseState -> ParseState
modifyVarEnv f (ParseState ve fe) = ParseState (f ve) fe

-- Applies f to the function environment only.
modifyFuncEnv :: (FuncEnv -> FuncEnv) -> ParseState -> ParseState
modifyFuncEnv f (ParseState ve fe) = ParseState ve (f fe)
