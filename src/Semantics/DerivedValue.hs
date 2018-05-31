module Semantics.DerivedValue where

import State.App
import State.Config as Config
import State.Error
import State.Tape as Tape (getSym)
import Syntax.Tree

-- The semantic function D[[.]] over tape symbols.
derivedVal :: (Monad m) => DerivedValue -> Config -> App m TapeSymbol
derivedVal (Literal sym)   _ = return sym
derivedVal (Var name)      c = tryMaybe (Config.getSym name c) (UndefVar name)
derivedVal (Read tapeName) c = tryMaybe getRead (UndefTape tapeName) where
    getRead = do
        tape <- getTape tapeName c
        return (Tape.getSym tape)
