module State.Machine where

-- A representation of the configuration of a Turing machine.
data Machine e a = HaltA e  -- The machine halted in the accepting state.
                 | HaltR e  -- The machine halted in the reject state.
                 | Inter a -- The machine is running.
                 deriving (Eq)

instance Functor (Machine e) where
    -- fmap :: (a -> b) -> Machine e a -> Machine e b
    fmap _ (HaltA a)   = HaltA a
    fmap _ (HaltR r)   = HaltR r
    fmap f (Inter x) = Inter (f x)

instance Applicative (Machine e) where
    -- pure :: a -> Machine e a
    pure = Inter
    -- (<*>) :: Machine e (a -> b) -> Machine e a -> Machine e b
    (HaltA a) <*> _    = HaltA a
    (HaltR r) <*> _    = HaltR r
    (Inter f) <*> mach = fmap f mach

instance Monad (Machine e) where
    -- (>>=) :: Machine e a -> (a -> Machine e b) -> Machine e b
    (HaltA a) >>= _ = HaltA a
    (HaltR r) >>= _ = HaltR r
    (Inter x) >>= f = f x

instance (Show e, Show a) => Show (Machine e a) where
    -- show :: (Machine e a) -> String
    show (HaltA a) = "Accepted: " ++ (show a)
    show (HaltR r) = "Rejected: " ++ (show r)
    show (Inter x) = "Inter: " ++ (show x)

-- Returns either `acc`, `rej`, or applies f depending on the state of the
-- machine.
machine :: (e -> b) -> (e -> b) -> (a -> b) -> Machine e a -> b
machine acc rej f m = case m of
    HaltA a -> acc a
    HaltR r -> rej r
    Inter x -> f x
