module Semantics.DerivedSymbol where

import State.App
import State.Config
import State.Error
import State.Tape as Tape (getSym)
import Syntax.Tree

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: (Monad m) => DerivedSymbol -> Config -> App m TapeSymbol
derivedSymbolVal (Literal sym)   _ = return sym
derivedSymbolVal (Var name)      c = tryMaybe (getVar name c) (UndefVar name)
derivedSymbolVal (Read tapeName) c = tryMaybe getRead (UndefTape tapeName) where
    getRead = do
        tape <- getTape tapeName c
        return (Tape.getSym tape)
