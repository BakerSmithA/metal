module Semantics.DerivedSymbol where

import Control.Exception
import State.App
import State.Config
import State.Error
import Syntax.Tree

-- Retrieves the value of a variable, throwing an undefined variable error if
-- the variable has not be defined.
getVarVal :: VarName -> Config -> App TapeSymbol
getVarVal name config = do
    let val = lookupVar name config
    maybe (throw (UndefVar name)) return val

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> Config -> App TapeSymbol
derivedSymbolVal (Read)        = return . getCurr
derivedSymbolVal (Var name)    = getVarVal name
derivedSymbolVal (Literal sym) = const (return sym)
