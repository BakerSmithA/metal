module Main where

import Test.Hspec
import Syntax.ParserSpec
import Semantics.StateSpec
import Semantics.DenotationalSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        stateSpec
        denotationalSpec
