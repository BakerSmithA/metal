module State.Machine where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- A representation of the configuration of a Turing machine.
data Machine a = HaltA   -- The machine halted in the accepting state.
               | HaltR   -- The machine halted in the reject state.
               | Inter a -- The machine is running.
               deriving (Eq)

instance Functor Machine where
    -- fmap :: (a -> b) -> Machine a -> Machine b
    fmap f (HaltA)   = HaltA
    fmap f (HaltR)   = HaltR
    fmap f (Inter x) = Inter (f x)

instance Applicative Machine where
    -- pure :: a -> Machine a
    pure = Inter
    -- (<*>) :: Machine (a -> b) -> Machine a -> Machine b
    (HaltA)   <*> mach = HaltA
    (HaltR)   <*> mach = HaltR
    (Inter f) <*> mach = fmap f mach

instance Monad Machine where
    -- (>>=) :: Machine a -> (a -> Machine b) -> Machine b
    (HaltA)   >>= f = HaltA
    (HaltR)   >>= f = HaltR
    (Inter x) >>= f = f x

instance (Show a) => Show (Machine a) where
    -- show :: (Machine a) -> String
    show (HaltA)   = "Accepted"
    show (HaltR)   = "Rejected"
    show (Inter x) = "Inter: " ++ (show x)

-- Returns either `acc`, `rej`, or applies f depending on the state of the
-- machine.
machine :: b -> b -> (a -> b) -> Machine a -> b
machine acc rej f m = case m of
    HaltA   -> acc
    HaltR   -> rej
    Inter x -> f x

data MachineT m a = MachineT {
    runMachineT :: m (Machine a)
}

instance (Functor m) => Functor (MachineT m) where
    -- fmap :: (a -> b) -> MachineT m a -> MachineT m b
    fmap f = MachineT . fmap (fmap f) . runMachineT

instance (Applicative m) => Applicative (MachineT m) where
    -- pure :: a -> MachineT m a
    pure = MachineT . pure . Inter
    -- (<*>) :: MachineT m (a -> b) -> MachineT m a -> MachineT m b
    mx <*> mf = MachineT $ liftA2 (<*>) (runMachineT mx) (runMachineT mf)

instance (Monad m) => Monad (MachineT m) where
    -- (>>=) :: MachineT m a -> (a -> MachineT m b) -> MachineT m b
    mx >>= f = MachineT $ runMachineT mx >>= machine (return HaltA) (return HaltR) (runMachineT . f)

instance MonadTrans MachineT where
    -- lift :: m a -> MachineT m a
    lift = MachineT . liftM Inter

-- Transforms the computation inside a `MachineT`.
mapMachineT :: (m (Machine a) -> n (Machine b)) -> MachineT m a -> MachineT n b
mapMachineT f = MachineT . f . runMachineT
