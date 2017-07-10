{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module State.MachineClass where

import Control.Monad.Writer
import State.App
import State.Machine
import State.Trans.Machine

-- Written with help from:
-- https://hackage.haskell.org/package/mtl-2.2.1/docs/src/Control.Monad.Error.Class.html#MonadError

-- By being an instance of the type, the functions can be used anywhere in a
-- monad stack.
class (Monad m) => MonadMachine a m where
    -- A machine in halted in the accepted state.
    accept :: m a
    -- A machine in halted in the rejected state.
    reject :: m a
    -- An intermediate state of a Turing machine.
    inter :: a -> m a

-- These requires FlexibleInstances, because it does not satisfy the coverage
-- condition. For more information see:
-- https://stackoverflow.com/questions/5941701/why-can-i-not-make-string-an-instance-of-a-typeclass

instance (Monad m) => MonadMachine a (MachineT m) where
    -- accept :: MachineT a
    accept = MachineT (return HaltA)
    -- reject :: MachineT a
    reject = MachineT (return HaltR)
    -- inter :: a -> MachineT a
    inter c = MachineT (return (Inter c))

instance MonadMachine a App where
    -- accept :: State a
    accept = App accept
    -- reject :: State a
    reject = App reject
    -- inter :: a -> State a
    inter = return

instance (Monoid w, Monad m, MonadMachine a m) => MonadMachine a (WriterT w m) where
    -- accept :: WriterT w m a
    accept = lift accept
    -- reject :: WriterT w m a
    reject = lift reject
    -- inter :: a -> WriterT w m a
    inter = return
