module State.ProgState where

import State.Env
import State.Machine
import Control.Applicative
import Control.Monad.Reader

-- type StateM a     = ReaderT Env Machine a --ReaderT Env (ExceptT RuntimeError Machine) a
-- type StateMConfig = StateM Config

newtype ProgStateT m a = ProgStateT { runState :: m (Machine a) }

instance (Functor m) => Functor (ProgStateT m) where
    -- fmap :: (a -> b) -> (ProgStateT a) -> (ProgStateT b)
    fmap f = ProgStateT . fmap (fmap f) . runState

instance (Applicative m) => Applicative (ProgStateT m) where
    -- pure :: a -> ProgStateT a
    pure = ProgStateT . pure . return
    -- (<*>) :: ProgStateT (a -> b) -> ProgStateT a -> ProgStateT b
    (ProgStateT f) <*> (ProgStateT s) = ProgStateT $ liftA2 (<*>) f s

instance (Monad m) => Monad (ProgStateT m) where
    -- (>>=) :: ProgStateT a -> (a -> ProgStateT b) -> ProgStateT b
    (ProgStateT s) >>= f = undefined

instance MonadTrans ProgStateT where
    -- lift :: ProgStateT -> t ProgStateT
    lift = undefined
