{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Semantics.Variable
( symVal
, tapePtr
, objPtr
, anyVal
) where

import State.App
import State.Config as Config
import State.Error
import State.Tape as Tape (fromString, getSym)
import Syntax.Tree

fmapFst :: (Functor f) => (a -> b) -> f (a, c) -> f (b, c)
fmapFst f z = fmap (\(x, y) -> (f x, y)) z

-- Follows the variable path to find the address of the object being referred to.
var :: (Monad m) => (Config -> Maybe a) -> Config -> App m (a, Config)
var get c = do
    x <- tryMaybe (get c) UndefVar
    return (x, c)

-- Returns the **value** of a new symbol expression, and the new config.
symVal :: (Monad m) => SymExpr -> Config -> App m (TapeSymbol, Config)
symVal (SymVar namePath) c = var (Config.getSym namePath) c
symVal (SymLit sym)      c = return (sym, c)
symVal (Read tapeExpr)   c = do
    (addr, c') <- tapePtr tapeExpr c
    (TapeRef tape) <- tryMaybe (Config.derefPtr addr c) UndefVar
    return (Tape.getSym tape, c')

-- Returns the address of a newly created tape, or an already exisiting tape,
-- and the config containing the tape.
tapePtr :: (Monad m) => TapeExpr -> Config -> App m (Address, Config)
tapePtr (TapeVar namePath) c = var (Config.getPtr namePath) c
tapePtr (TapeLit syms)     c = return (Config.putRef tapeRef c) where
    tapeRef = TapeRef (Tape.fromString syms)

-- Returns the address of the newly created object, or an existing object,
-- and the config containing the object.s
objPtr :: (Monad m) => ObjExpr -> Config -> App m (Address, Config)
objPtr (ObjVar _ namePath)         c = var (Config.getPtr namePath) c
objPtr (NewObj structName memArgs) c = do
    memNames <- tryMaybe (Config.getStructMems structName c) (UndefStruct structName)
    undefined

-- Returns the value of an expression which may evaluate to either a tape
-- symbol, tape, or object.
anyVal :: (Monad m) => AnyValExpr -> Config -> App m (Variable, Config)
anyVal (S symExpr)  = fmapFst Symbol . symVal  symExpr
anyVal (T tapeExpr) = fmapFst Ptr    . tapePtr tapeExpr
anyVal (C objExpr)  = fmapFst Ptr    . objPtr  objExpr
