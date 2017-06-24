module Main where

import Syntax.ParserSpec
import State.ConfigSpec
import State.EnvSpec
import State.TapeSpec
import Semantics.DenotationalSpec
import Test.Hspec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        configSpec
        envSpec
        tapeSpec
        derivedSymbolValSpec
        bexpValSpec
        denotationalSpec
