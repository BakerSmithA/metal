{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module State.MachineClass where

-- This module was constructed help from looking at MonadError:
-- https://hackage.haskell.org/package/mtl-2.2.1/docs/src/Control.Monad.Error.Class.html#MonadError

import Control.Monad.Except
import Control.Monad.Reader
import State.Machine

-- By being an instance of the type, the functions can be used anywhere in a
-- monad stack.
class (Monad m) => MonadMachine a m where
    -- A machine in halted in the accepted state.
    accept :: m a
    -- A machine in halted in the rejected state.
    reject :: m a
    -- An intermediate state of a Turing machine.
    inter :: a -> m a
