{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.App where

import Control.Monad.IO.Class
import State.Machine
import State.Trans.Machine

newtype App m a = App {
    runApp :: MachineT m a
} deriving (Functor
          , Applicative
          , Monad
          , MonadIO)

-- Output the string `str` without changing the state of the program.
output :: String -> a -> App m a
output = undefined
-- output str x = do
--     liftIO (putStrLn str)
--     return x

-- Runs the program in the given environment.
evalApp :: App m a -> m (Machine a)
evalApp p = runMachineT (runApp p)
