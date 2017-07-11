{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.App where

import Control.Monad.Except
import Control.Monad.Writer
import State.Error
import State.Machine
import State.Trans.Machine

newtype App a = App {
    runApp :: MachineT IO a
} deriving (Functor
          , Applicative
          , Monad
          , MonadIO)

-- Adds `str` to the list of outputted strings.
output :: String -> a -> App a
output str x = do
    liftIO (putStrLn str)
    return x

-- Runs the program in the given environment.
--evalApp :: App a -> Either RuntimeError (Machine (a, [String]))
evalApp :: App a -> IO (Machine a)
evalApp p = runMachineT (runApp p)
