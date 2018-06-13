module Main where

import Syntax.BexpSpec
import Syntax.CommonSpec
import Syntax.ControlSpec
import Syntax.FuncSpec
import Syntax.IdentifierSpec
import Syntax.ParserSpec
import Syntax.VariableExprSpec
import State.TapeSpec
import State.ConfigSpec
import State.TreeSpec
import Semantics.BexpSpec
import Semantics.StmSpec
import Semantics.ProgramSpec
import Test.Hspec

main :: IO ()
main = hspec specs where
    specs = do
        bexpSpec
        commonSpec
        controlSpec
        funcSpec
        identifierSpec
        parserSpec
        variableExprSpec
        tapeSpec
        configSpec
        treeSpec
        bexpValSpec
        stmSpec
        programSpec
