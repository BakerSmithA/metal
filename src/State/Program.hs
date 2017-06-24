{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.Program where

import Control.Monad.Except
import Control.Monad.Reader
import State.Config
import State.Error
import State.Machine
import State.MachineClass
import State.Trans.Machine

newtype Prog a = Prog {
    runProg :: MachineT (ExceptT RuntimeError IO) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadIO
          , MonadError RuntimeError
          , MonadMachine a)

type ProgConfig = Prog Config

-- Runs the program in the given environment.
runProgram :: Prog a -> IO (Either RuntimeError (Machine a))
runProgram p = runExceptT (runMachineT (runProg p))
