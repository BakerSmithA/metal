{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module State.MachineClass where

-- By being an instance of the type, the functions can be used anywhere in a
-- monad stack.
class (Monad m) => MonadMachine a m where
    -- A machine in halted in the accepted state.
    accept :: m a
    -- A machine in halted in the rejected state.
    reject :: m a
    -- An intermediate state of a Turing machine.
    inter :: a -> m a
