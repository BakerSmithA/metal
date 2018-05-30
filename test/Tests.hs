module Main where

import Syntax.ParserSpec
import State.TapeSpec
import State.ConfigSpec
import Semantics.DerivedSymbolSpec
import Semantics.BexpSpec
import Semantics.StmSpec
import Semantics.ProgramSpec
import Test.Hspec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        tapeSpec
        configSpec
        derivedSymbolValSpec
        bexpValSpec
        stmSpec
        programSpec
