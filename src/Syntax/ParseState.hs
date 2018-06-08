module Syntax.ParseState where

import Syntax.Env as E
import Syntax.Tree (VarName, FuncName, StructName, StructMember, DataType)
import Data.Map as Map

-- Stores variable identifiers and their associated types.
type VarEnv = Env DataType
-- Stores function identifiers and their associated argument types.
type FuncEnv = Env [DataType]
-- Stores struct names and their associated member variable names and types.
type StructEnv = Env [StructMember]

-- Keeps track of used variable and function names to avoid invalid programs.
data ParseState = ParseState {
    varEnv :: VarEnv,
    funcEnv :: FuncEnv,
    structEnv :: StructEnv
}

-- A parse state containing no variables or functions.
empty :: ParseState
empty = ParseState E.empty E.empty E.empty

-- A parse state containing variables and functions with the given names.
fromLists :: [(VarName, DataType)] -> [(FuncName, [DataType])] -> ParseState
fromLists vars funcs = ParseState (E.fromList vars) (E.fromList funcs) E.empty

-- A parse state containing only variables with the given names.
fromVarList :: [(VarName, DataType)] -> ParseState
fromVarList vars = fromLists vars []

-- A parse state containing only functions with the given names.
fromFuncList :: [(FuncName, [DataType])] -> ParseState
fromFuncList funcs = fromLists [] funcs

-- Applies f to the variable environment only.
mapVarEnv :: (VarEnv -> VarEnv) -> ParseState -> ParseState
mapVarEnv f env = env { varEnv = f (varEnv env) }

-- Applies f to the function environment only.
mapFuncEnv :: (FuncEnv -> FuncEnv) -> ParseState -> ParseState
mapFuncEnv f env = env { funcEnv = f (funcEnv env) }

-- Applies f to the struct environment only.
mapStructEnv :: (StructEnv -> StructEnv) -> ParseState -> ParseState
mapStructEnv f env = env { structEnv = f (structEnv env) }
