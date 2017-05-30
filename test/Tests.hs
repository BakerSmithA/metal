module Main where

import Test.Hspec
import Syntax.ParserSpec
import Semantics.DenotationalSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        denotationalSpec
