module Semantics.BexpSpec where

import State.Config as Config
import State.Tape()
import Syntax.Tree
import TestHelper.Denotational
import Test.Hspec hiding (shouldReturn, shouldThrow, shouldContain)

bexpValSpec :: Spec
bexpValSpec = do
    let testConfig  = right (Config.fromString "abc")
        testConfig' = addVar "x" '1' testConfig

    describe "bexpVal" $ do
        it "evaluates TRUE" $ do
            let result = evalBexp TRUE testConfig'
            result `shouldReturn` True

        it "evaluates FALSE" $ do
            let result = evalBexp FALSE testConfig'
            result `shouldReturn` False

        it "evaluates not" $ do
            let result = evalBexp (Not TRUE) testConfig'
            result `shouldReturn` False

        it "evaluates and" $ do
            let ff = evalBexp (And FALSE FALSE) testConfig'
                ft = evalBexp (And FALSE TRUE) testConfig'
                tf = evalBexp (And TRUE FALSE) testConfig'
                tt = evalBexp (And TRUE TRUE) testConfig'

            ff `shouldReturn` False
            ft `shouldReturn` False
            tf `shouldReturn` False
            tt `shouldReturn` True

        it "evaluates or" $ do
            let ff = evalBexp (Or FALSE FALSE) testConfig'
                ft = evalBexp (Or FALSE TRUE) testConfig'
                tf = evalBexp (Or TRUE FALSE) testConfig'
                tt = evalBexp (Or TRUE TRUE) testConfig'

            ff `shouldReturn` False
            ft `shouldReturn` True
            tf `shouldReturn` True
            tt `shouldReturn` True

        it "evaluates <=" $ do
            let b1      = Le (Read) (Literal 'c') -- The current symbol is 'b'.
                b2      = Le (Read) (Literal 'a')
                result1 = evalBexp b1 testConfig'
                result2 = evalBexp b2 testConfig'

            result1 `shouldReturn` True
            result2 `shouldReturn` False

        it "evaluates ==" $ do
            let b1      = Eq (Read) (Literal 'b') -- The current symbol is 'b'.
                b2      = Eq (Read) (Literal '#')
                result1 = evalBexp b1 testConfig'
                result2 = evalBexp b2 testConfig'

            result1 `shouldReturn` True
            result2 `shouldReturn` False

        it "evaluates !=" $ do
            let b1      = Ne (Read) (Literal 'b') -- The current symbol is 'b'.
                b2      = Ne (Read) (Literal '#')
                result1 = evalBexp b1 testConfig'
                result2 = evalBexp b2 testConfig'

            result1 `shouldReturn` False
            result2 `shouldReturn` True
