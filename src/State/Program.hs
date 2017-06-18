{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.Program where

import Control.Monad.Except
import Control.Monad.Reader
import State.Config
import State.Env
import State.Error
import State.MachineClass
import State.Trans.Machine

newtype Prog a = Prog {
    runProg :: ReaderT Env (ExceptT RuntimeError (MachineT IO)) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadIO
          , MonadReader Env
          , MonadError RuntimeError
          , MonadMachine a)

type ProgConfig = Prog Config
