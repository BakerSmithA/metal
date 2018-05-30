{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.App where

import Control.Exception
import Control.Monad.Trans.Class
import State.Error
import State.Machine
import State.Trans.Machine
import State.Output
import State.Tape

newtype App m a = App {
    runApp :: MachineT m a
} deriving (Functor
          , Applicative
          , Monad
          , MonadTrans)

-- Output the string `str` without changing the state of the program.
output' :: (MonadOutput m) => String -> a -> App m a
output' str x = do
    lift (output str)
    return x

-- Runs the program in the given environment.
evalApp :: App m a -> m (Machine a)
evalApp p = runMachineT (runApp p)

-- Converts from Maybe to App.
tryMaybe :: (Monad m) => Maybe a -> RuntimeError -> App m a
tryMaybe x err = maybe (throw err) return x
