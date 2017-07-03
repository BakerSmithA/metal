module Semantics.BexpSpec where

import State.Config as Config
import State.Tape()
import Syntax.Tree
import TestHelper.Denotational
import Test.Hspec hiding (shouldContain, shouldThrow)

bexpValSpec :: Spec
bexpValSpec = do
    let testConfig  = right (Config.fromString "abc")
        testConfig' = addVar "x" '1' testConfig

    describe "bexpVal" $ do
        it "evaluates TRUE" $ do
            let result = evalBexp TRUE testConfig'
            result `shouldContain` True

        it "evaluates FALSE" $ do
            let result = evalBexp FALSE testConfig'
            result `shouldContain` False

        it "evaluates not" $ do
            let result = evalBexp (Not TRUE) testConfig'
            result `shouldContain` False

        it "evaluates and" $ do
            let ff = evalBexp (And FALSE FALSE) testConfig'
                ft = evalBexp (And FALSE TRUE) testConfig'
                tf = evalBexp (And TRUE FALSE) testConfig'
                tt = evalBexp (And TRUE TRUE) testConfig'

            ff `shouldContain` False
            ft `shouldContain` False
            tf `shouldContain` False
            tt `shouldContain` True

        it "evaluates or" $ do
            let ff = evalBexp (Or FALSE FALSE) testConfig'
                ft = evalBexp (Or FALSE TRUE) testConfig'
                tf = evalBexp (Or TRUE FALSE) testConfig'
                tt = evalBexp (Or TRUE TRUE) testConfig'

            ff `shouldContain` False
            ft `shouldContain` True
            tf `shouldContain` True
            tt `shouldContain` True

        it "evaluates <=" $ do
            let b1      = Le (Read) (Literal 'c') -- The current symbol is 'b'.
                b2      = Le (Read) (Literal 'a')
                result1 = evalBexp b1 testConfig'
                result2 = evalBexp b2 testConfig'

            result1 `shouldContain` True
            result2 `shouldContain` False

        it "evaluates ==" $ do
            let b1      = Eq (Read) (Literal 'b') -- The current symbol is 'b'.
                b2      = Eq (Read) (Literal '#')
                result1 = evalBexp b1 testConfig'
                result2 = evalBexp b2 testConfig'

            result1 `shouldContain` True
            result2 `shouldContain` False

        it "evaluates !=" $ do
            let b1      = Ne (Read) (Literal 'b') -- The current symbol is 'b'.
                b2      = Ne (Read) (Literal '#')
                result1 = evalBexp b1 testConfig'
                result2 = evalBexp b2 testConfig'

            result1 `shouldContain` False
            result2 `shouldContain` True
