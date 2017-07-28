{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.App where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import State.Machine
import State.Trans.Machine
import State.Output

newtype App m a = App {
    runApp :: MachineT m a
} deriving (Functor
          , Applicative
          , Monad
          , MonadIO
          , MonadTrans)

-- Output the string `str` without changing the state of the program.
output' :: (MonadOutput m) => String -> a -> App m a
output' str x = do
    lift (output str)
    return x

-- Runs the program in the given environment.
evalApp :: App m a -> m (Machine a)
evalApp p = runMachineT (runApp p)
