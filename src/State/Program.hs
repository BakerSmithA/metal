{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.Program where

import Control.Monad.Except
import Control.Monad.Reader
import State.Config
import State.Env
import State.Error
import State.Machine
import State.MachineClass
import State.Trans.Machine

newtype Prog a = Prog {
    runProg :: ReaderT Env (MachineT (Either RuntimeError)) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadReader Env
          , MonadError RuntimeError
          , MonadMachine a)

type ProgConfig = Prog Config

-- Runs the program in the given environment.
runProgram :: Prog a -> Env -> Either RuntimeError (Machine a)
runProgram p env = runMachineT (runReaderT (runProg p) env)
