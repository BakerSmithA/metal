{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Semantics.Variable
( symVal
, tapeVal
) where

import State.App
import State.Config as Config
import State.Error
import State.Tape
import State.Tape as Tape (fromString, getSym)
import Syntax.Tree hiding (Tape)
import qualified Syntax.Tree as Syn (Tape)

newSymVal :: (Monad m) => Sym -> Config -> App m TapeSymbol
newSymVal (SymLit sym)    c = return sym
newSymVal (Read tapeExpr) c = tapeVal tapeExpr c >>= return . Tape.getSym

newTapeVal :: (Monad m) => Syn.Tape -> Config -> App m Tape
newTapeVal (TapeLit syms) _ = return (Tape.fromString syms)

symVal :: (Monad m) => Val Sym -> Config -> App m TapeSymbol
symVal (New x)    c = newSymVal x c
symVal (Var name) c = tryMaybe (Config.getSym name c) (UndefVar name)

tapeVal :: (Monad m) => Val Syn.Tape -> Config -> App m Tape
tapeVal (New x)    c = newTapeVal x c
tapeVal (Var name) c = tryMaybe (Config.getTape name c) (UndefTape name)
