module Semantics.Helpers where

import State.App
import State.Config

-- Fixpoint operator used to defined loops.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(Config -> App Bool, Config -> App Config)] -> (Config -> App Config)
cond []                       config = return config
cond ((predicate, branch):ps) config = do
    bVal <- predicate config
    if bVal then branch config
            else cond ps config

-- Performs `f` on the program ensuing changes to the variable or function
-- environment are not persistented outside the block. I.e. after finishing
-- executing the statement, the variable and function environments return to
-- how they were before the statement.
block :: (Config -> App Config) -> Config -> App Config
block f oldConfig = do
    newConfig <- f oldConfig
    return (resetEnv oldConfig newConfig)
