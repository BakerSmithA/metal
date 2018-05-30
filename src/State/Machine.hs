module State.Machine where

-- A representation of the configuration of a Turing machine.
data Machine a = HaltA   -- The machine halted in the accepting state.
               | HaltR   -- The machine halted in the reject state.
               | Inter a -- The machine is running.
               deriving (Eq)

instance Functor Machine where
    -- fmap :: (a -> b) -> Machine a -> Machine b
    fmap _ (HaltA)   = HaltA
    fmap _ (HaltR)   = HaltR
    fmap f (Inter x) = Inter (f x)

instance Applicative Machine where
    -- pure :: a -> Machine a
    pure = Inter
    -- (<*>) :: Machine (a -> b) -> Machine a -> Machine b
    (HaltA)   <*> _    = HaltA
    (HaltR)   <*> _    = HaltR
    (Inter f) <*> mach = fmap f mach

instance Monad Machine where
    -- (>>=) :: Machine a -> (a -> Machine b) -> Machine b
    (HaltA)   >>= _ = HaltA
    (HaltR)   >>= _ = HaltR
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
