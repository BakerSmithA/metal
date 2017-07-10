{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State.App where

import Control.Monad.Except
import State.Error
import State.Machine
import State.MachineClass
import State.Trans.Machine

newtype App a = App {
    runApp :: MachineT (Either RuntimeError) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadError RuntimeError)

instance MonadMachine a App where
    -- accept :: State a
    accept = App accept
    -- reject :: State a
    reject = App reject
    -- inter :: a -> State a
    inter = return

-- Runs the program in the given environment.
runApp' :: App a -> Either RuntimeError (Machine a)
runApp' p = runMachineT (runApp p)
