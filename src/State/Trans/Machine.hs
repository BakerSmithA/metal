{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State.Trans.Machine where

import Control.Applicative
import Control.Monad.Except
import State.Machine

-- A monad transformer which adds Machine semantics to an existing monad.
data MachineT m a = MachineT {
    runMachineT :: m (Machine a)
}

instance (Functor m) => Functor (MachineT m) where
    -- fmap :: (a -> b) -> MachineT m a -> MachineT m b
    fmap f = MachineT . fmap (fmap f) . runMachineT

instance (Applicative m) => Applicative (MachineT m) where
    -- pure :: a -> MachineT m a
    pure = MachineT . pure . Inter
    -- (<*>) :: MachineT m (a -> b) -> MachineT m a -> MachineT m b
    mx <*> mf = MachineT $ liftA2 (<*>) (runMachineT mx) (runMachineT mf)

instance (Monad m) => Monad (MachineT m) where
    -- (>>=) :: MachineT m a -> (a -> MachineT m b) -> MachineT m b
    mx >>= f = MachineT $ runMachineT mx >>= machine (return HaltA) (return HaltR) (runMachineT . f)

instance MonadTrans MachineT where
    -- lift :: m a -> MachineT m a
    lift = MachineT . liftM Inter

instance (MonadIO m) => MonadIO (MachineT m) where
    -- liftIO :: IO a -> m a
    liftIO = lift . liftIO

-- This requires UndecidableInstances, because it does not satisfy the coverage
-- condition.
instance MonadError e m => MonadError e (MachineT m) where
    --  throwError :: e -> m a
    throwError = undefined
    -- catchError :: m a -> (e -> m a) -> m a
    catchError = undefined
