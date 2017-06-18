{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State.Program where

import State.Env as Env
import State.Machine
import State.MachineMonad
import State.Error
import Control.Monad.Reader
import Control.Monad.Except

-- newtype Prog a = Prog {
--     runState :: ReaderT Env (ExceptT RuntimeError (MachineT IO)) a
-- } deriving (Functor, Applicative, Monad)

type Prog a = ReaderT Env (ExceptT RuntimeError (MachineT IO)) a
