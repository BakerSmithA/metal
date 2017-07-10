module Semantics.DerivedSymbol where

import Control.Monad.Except hiding (fix)
import State.App
import State.Config
import State.Error
import Syntax.Tree

-- Retrieves the value of a variable, throwing an undefined variable error if
-- the variable has not be defined.
getVarVal :: VarName -> App Config -> App TapeSymbol
getVarVal name p = do
    config <- p
    let val = lookupVar name config
    maybe (throwError (UndefVar name)) return val

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> App Config -> App TapeSymbol
derivedSymbolVal (Read)        = fmap getCurr
derivedSymbolVal (Var name)    = getVarVal name
derivedSymbolVal (Literal sym) = const (return sym)
