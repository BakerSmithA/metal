module Main where

import Syntax.Tree
import Semantics.Denotational

main :: IO ()
main = do
    let stm = (Comp (PrintStr "STR") Accept)
    return ()
