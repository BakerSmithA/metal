module Semantics.Helpers where

import State.App
import State.Config

-- Fixpoint operator used to defined loops.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: (Monad m) => [(Config -> App m (Bool, Config), Config -> App m Config)] -> (Config -> App m Config)
cond []                       c = return c
cond ((predicate, branch):ps) c = do
    (bVal, c') <- predicate c
    if bVal then branch c'
            else cond ps c'

-- Performs `f` on the program ensuing changes to the variable or function
-- environment are not persistented outside the block. I.e. after finishing
-- executing the statement, the variable and function environments return to
-- how they were before the statement.
block :: (Monad m) => (Config -> App m Config) -> Config -> App m Config
block f oldConfig = do
    newConfig <- f oldConfig
    return (revertEnv oldConfig newConfig)
