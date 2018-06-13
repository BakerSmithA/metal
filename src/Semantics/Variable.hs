{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Semantics.Variable where

import State.App
import State.Config as Config
import State.Error
import State.Tape as Tape (fromString, getSym)
import Syntax.Tree

-- Returns the **value** of a new symbol expression, and the new config.
symVal :: (Monad m) => SymExpr -> Config -> App m (TapeSymbol, Config)
symVal (SymLit sym)    c = return (sym, c)
symVal (Read tapeExpr) c = do
    (addr, c') <- tapePtr tapeExpr c
    (TapeRef tape) <- tryMaybe (Config.derefPtr addr c) UndefVar
    return (Tape.getSym tape, c')
symVal (SymVar namePath) c = do
    sym <- tryMaybe (Config.getSym namePath c) UndefVar
    return (sym, c)

-- Returns the address of a newly created tape, or an already exisiting tape,
-- and the config containing the tape.
tapePtr :: (Monad m) => TapeExpr -> Config -> App m (Address, Config)
tapePtr (TapeLit syms) c = return (Config.putRef (TapeRef $ Tape.fromString syms) c)
tapePtr (TapeVar namePath) c = do
    addr <- tryMaybe (Config.getPtr namePath c) UndefVar
    return (addr, c)
