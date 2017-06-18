module State.Env
( Env
, initial
, lookupVar
, lookupFunc
, addVar
, addFunc
) where

import Data.Map as Map
import Syntax.Tree

-- A list of variable definitions which maps variable names to tape symbols.
type VarDefs = Map VarName TapeSymbol

-- A function definitions which maps function names to function bodies.
type FuncDefs = Map FuncName Stm

-- The final environment consists of both a variable and function definitions.
data Env = Env {
    vars  :: VarDefs
  , funcs :: FuncDefs
}

-- An empty environment containing no variable or function definitions.
initial :: Env
initial = Env Map.empty Map.empty

-- Looks up a variable in an environment.
lookupVar :: VarName -> Env -> Maybe TapeSymbol
lookupVar name env = Map.lookup name (vars env)

-- Looks up a function in an environment.
lookupFunc :: FuncName -> Env -> Maybe Stm
lookupFunc name env = Map.lookup name (funcs env)

-- Adds a single variable to the environment.
addVar :: VarName -> TapeSymbol -> Env -> Env
addVar name sym env = env { vars = insert name sym (vars env) }

-- Adds a single function to the environment.
addFunc :: FuncName -> Stm -> Env -> Env
addFunc name body env = env { funcs = insert name body (funcs env) }
