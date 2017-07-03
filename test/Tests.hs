module Main where

import Syntax.ParserSpec
import State.ConfigSpec
import State.TapeSpec
import Semantics.BexpSpec
import Semantics.DerivedSymbolSpec
import Semantics.StmSpec
import Test.Hspec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        configSpec
        tapeSpec
        derivedSymbolValSpec
        bexpValSpec
        denotationalSpec
