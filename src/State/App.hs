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
    -- accept :: App a
    accept = App accept
    -- reject :: App a
    reject = App reject
    -- inter :: a -> App a
    inter = return

-- Runs the program in the given environment.
runAppram :: App a -> Either RuntimeError (Machine a)
runAppram p = runMachineT (runApp p)
