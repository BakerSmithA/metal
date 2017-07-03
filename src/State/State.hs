{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State.State where

import Control.Monad.Except
import State.Error
import State.Machine
import State.MachineClass
import State.Trans.Machine

newtype State a = State {
    runState :: MachineT (Either RuntimeError) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadError RuntimeError)

instance MonadMachine a State where
    -- accept :: State a
    accept = State accept
    -- reject :: State a
    reject = State reject
    -- inter :: a -> State a
    inter = return

-- Runs the program in the given environment.
runState' :: State a -> Either RuntimeError (Machine a)
runState' p = runMachineT (runState p)
