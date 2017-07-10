{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.App where

import Control.Monad.Except
import Control.Monad.Writer
import State.Error
import State.Machine
import State.Trans.Machine

newtype App a = App {
    runApp :: WriterT [String] (MachineT (Either RuntimeError)) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadError RuntimeError)

-- Runs the program in the given environment.
evalApp :: App a -> Either RuntimeError (Machine (a, [String]))
evalApp p = runMachineT (runWriterT (runApp p))
