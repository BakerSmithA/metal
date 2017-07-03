{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State.Program where

import Control.Monad.Except
import State.Error
import State.Machine
import State.MachineClass
import State.Trans.Machine

newtype Prog a = Prog {
    runProg :: MachineT (Either RuntimeError) a
} deriving (Functor
          , Applicative
          , Monad
          , MonadError RuntimeError)

instance MonadMachine a Prog where
    -- accept :: Prog a
    accept = Prog accept
    -- reject :: Prog a
    reject = Prog reject
    -- inter :: a -> Prog a
    inter = return

-- Runs the program in the given environment.
runProgram :: Prog a -> Either RuntimeError (Machine a)
runProgram p = runMachineT (runProg p)
