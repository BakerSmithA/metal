module Semantics.Helpers where

import State.App
import State.Config

type AppConfig = App Config

-- Fixpoint operator used to defined loops.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(AppConfig -> App Bool, AppConfig -> AppConfig)] -> (AppConfig -> AppConfig)
cond []                       p = p
cond ((predicate, branch):ps) p = do
    bVal <- predicate p
    if bVal then branch p
            else cond ps p

-- Performs `f` on the program ensuing changes to the variable or function
-- environment are not persistented outside the block. I.e. after finishing
-- executing the statement, the variable and function environments return to
-- how they were before the statement.
block :: (AppConfig -> AppConfig) -> AppConfig -> AppConfig
block f p = do
    oldConfig <- p
    newConfig <- f p
    return (resetEnv oldConfig newConfig)
