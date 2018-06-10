{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Semantics.Variable
( symVal
, tapePtr
) where

import State.App
import State.Config as Config
import State.Error
import State.Tape as Tape (fromString, getSym)
import Syntax.Tree hiding (Tape)
import qualified Syntax.Tree as Syn (Tape)

-- Returns the **value** of a new symbol expression, and the new config.
newSymVal :: (Monad m) => Sym -> Config -> App m (TapeSymbol, Config)
newSymVal (SymLit sym)    c = return (sym, c)
newSymVal (Read tapeExpr) c = do
    (addr, c') <- tapePtr tapeExpr c
    tape <- tryMaybe (Config.derefTape addr c) UndefVar
    return (Tape.getSym tape, c')

-- newSymVal (Read tapeExpr) c = tapeVal tapeExpr c >>= return . Tape.getSym

-- Returns the **value** of a symbol expression (i.e. a variable or new symbol),
-- and the new config.
symVal :: (Monad m) => Val Sym -> Config -> App m (TapeSymbol, Config)
symVal (New x)    c = newSymVal x c
symVal (Var name) c = do
    sym <- tryMaybe (Config.getSym name c) UndefVar
    return (sym, c)

-- Returns the address of a newly created, unnamed, tape. Also returns the
-- config containing the tape.
newTapePtr :: (Monad m) => Syn.Tape -> Config -> App m (Address, Config)
newTapePtr (TapeLit syms) = return . Config.putTape (Tape.fromString syms)

-- Returns the address of a newly created tape, or an already exisiting tape,
-- and the config containing the tape.
tapePtr :: (Monad m) => Val Syn.Tape -> Config -> App m (Address, Config)
tapePtr (New x)    c = newTapePtr x c
tapePtr (Var name) c = do
    addr <- tryMaybe (Config.getTapePtr name c) UndefVar
    return (addr, c)
