module Semantics.DerivedSymbol where

import Control.Monad.Except hiding (fix)
import Semantics.Helpers
import State.Config
import State.Error
import State.App
import Syntax.Tree

-- Retrieves the value of a variable, throwing an undefined variable error if
-- the variable has not be defined.
getVarVal :: VarName -> AppConfig -> App TapeSymbol
getVarVal name p = do
    config <- p
    let val = lookupVar name config
    maybe (throwError (UndefVar name)) return val

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> AppConfig -> App TapeSymbol
derivedSymbolVal (Read)        = fmap getCurr
derivedSymbolVal (Var name)    = getVarVal name
derivedSymbolVal (Literal sym) = const (return sym)
