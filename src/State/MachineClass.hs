{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module State.MachineClass where

-- This module was constructed help from looking at MonadError:
-- https://hackage.haskell.org/package/mtl-2.2.1/docs/src/Control.Monad.Error.Class.html#MonadError

import Control.Monad.Except
import Control.Monad.Reader
import State.Machine
import State.Trans.Machine

-- A small aside on the language, below m -> a is read as the monad `m`
-- determines `a` type.

-- By being an instance of the type, the functions can be used anywhere in a
-- monad stack.
class (Monad m) => MonadMachine a m where
    -- A machine in halted in the accepted state.
    accept :: m a
    -- A machine in halted in the rejected state.
    reject :: m a
    -- An intermediate state of a Turing machine.
    inter :: a -> m a

-- ---------------------------------------------------------------------------
-- Instances for transformers, these instances need FlexibleInstances, because
-- they do not satisfy the coverage condition. For more information see:
-- https://stackoverflow.com/questions/5941701/why-can-i-not-make-string-an-instance-of-a-typeclass

instance (Monad m) => MonadMachine a (MachineT m) where
    -- accept :: MachineT a
    accept = MachineT (return HaltA)
    -- reject :: MachineT a
    reject = MachineT (return HaltR)
    -- inter :: a -> MachineT a
    inter c = MachineT (return (Inter c))

instance (MonadMachine a m) => MonadMachine a (ReaderT r m) where
    -- accept :: ReaderT r a
    accept = lift accept
    -- reject :: ReaderT r a
    reject = lift reject
    -- inter :: a -> ReaderT r a
    inter = lift . inter
