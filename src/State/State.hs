module State.State where

import State.Env
import State.Machine
import Control.Applicative
import Control.Monad.Reader

-- type StateM a     = ReaderT Env Machine a --ReaderT Env (ExceptT RuntimeError Machine) a
-- type StateMConfig = StateM Config

newtype ProgState a = ProgState { runState :: ReaderT Env Machine a }

runProgState :: ProgState a -> Env -> Machine a
runProgState s env = runReaderT (runState s) env

instance Functor ProgState where
    -- fmap :: (a -> b) -> (ProgState a) -> (ProgState b)
    fmap f = ProgState . fmap f . runState

instance Applicative ProgState where
    -- pure :: a -> ProgState a
    pure = ProgState . return
    -- (<*>) :: ProgState (a -> b) -> ProgState a -> ProgState b
    (ProgState f) <*> (ProgState s) = ProgState (f <*> s)

instance Monad ProgState where
    -- (>>=) :: ProgState a -> (a -> ProgState b) -> ProgState b
    (ProgState s) >>= f = undefined
