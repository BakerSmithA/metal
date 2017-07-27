module Semantics.DerivedSymbol where

import Control.Exception
import State.App
import State.Config
import State.Error
import Syntax.Tree

-- Retrieves the value of a variable, throwing an undefined variable error if
-- the variable has not be defined.
getVarVal :: (Monad m) => VarName -> Config -> App m TapeSymbol
getVarVal name config = do
    let val = lookupVar name config
    maybe (throw (UndefVar name)) return val

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: (Monad m) => DerivedSymbol -> Config -> App m TapeSymbol
derivedSymbolVal (Read)        = return . getCurr
derivedSymbolVal (Var name)    = getVarVal name
derivedSymbolVal (Literal sym) = const (return sym)
