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
    runProg :: MachineT (Either RuntimeError) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadError RuntimeError
          , MonadMachine a)

-- Runs the program in the given environment.
runProgram :: Prog a -> Either RuntimeError (Machine a)
runProgram p = runMachineT (runProg p)
