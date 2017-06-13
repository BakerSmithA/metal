module Main where

import Syntax.ParserSpec
import State.MachineSpec
import State.StateSpec
import Test.Hspec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        machineSpec
        stateSpec
