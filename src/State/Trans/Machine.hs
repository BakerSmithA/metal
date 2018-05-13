{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module State.Trans.Machine where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Signatures
import State.Machine

-- A monad transformer which adds Machine semantics to an existing monad.
data MachineT e m a = MachineT {
    runMachineT :: m (Machine e a)
}

instance (Functor m) => Functor (MachineT e m) where
    -- fmap :: (a -> b) -> MachineT e m a -> MachineT e m b
    fmap f = MachineT . fmap (fmap f) . runMachineT

instance (Applicative m) => Applicative (MachineT e m) where
    -- pure :: a -> MachineT e m a
    pure = MachineT . pure . Inter
    -- (<*>) :: MachineT e m (a -> b) -> MachineT e m a -> MachineT e m b
    mx <*> mf = MachineT $ liftA2 (<*>) (runMachineT mx) (runMachineT mf)

instance (Monad m) => Monad (MachineT e m) where
    -- (>>=) :: MachineT e m a -> (a -> MachineT e m b) -> MachineT e m b
    mx >>= f = MachineT $ runMachineT mx >>= machine (\e -> return $ HaltA e) (\e -> return $ HaltR e) (runMachineT . f)

instance MonadTrans (MachineT e) where
    -- lift :: m a -> MachineT e m a
    lift = MachineT . liftM Inter

instance (MonadIO m) => MonadIO (MachineT e m) where
    -- liftIO :: IO a -> m a
    liftIO = lift . liftIO

-- This requires UndecidableInstances, because it does not satisfy the coverage
-- condition.
instance MonadError e m => MonadError e (MachineT t m) where
    --  throwError :: e -> MachineT t m a
    throwError = lift . throwError
    -- catchError :: MachineT m a -> (e -> MachineT t m a) -> MachineT t m a
    catchError = liftCatch catchError

-- Lift a catchE operation to the new monad.
liftCatch :: Catch e m (Machine t a) -> Catch e (MachineT t m) a
liftCatch catch mach errHandler = MachineT $ catch (runMachineT mach) (runMachineT . errHandler)
