module State.TapeSpec (tapeSpec) where

import State.Tape
import Syntax.Tree
import TestHelper.Tape
import Test.Hspec

-- Asserts that the tape should contain `syms` at its start.
shouldRead :: Tape -> [TapeSymbol] -> Expectation
shouldRead tape syms = tapeShouldRead tape syms `shouldBe` True

tapeSpec :: Spec
tapeSpec = do
    fromStringSpec
    getSymSpec
    setSymSpec

fromStringSpec :: Spec
fromStringSpec = do
    describe "fromString" $ do
        it "places a string at the start of the tape" $ do
            let tape = fromString "abc"
            tape `shouldRead` "abc"

        it "leaves the rest of the tape as space characters" $ do
            let tape = fromString "abc"
            getSym 3 tape `shouldBe` ' '

getSymSpec :: Spec
getSymSpec = do
    describe "getSym" $ do
        it "returns the symbol at the specified position on the tape" $ do
            let tape = fromString "abc"
            getSym 0 tape `shouldBe` 'a'
            getSym 1 tape `shouldBe` 'b'
            getSym 2 tape `shouldBe` 'c'

        it "returns the blank (space) character if the cell is empty" $ do
            let tape = fromString "abc"
            getSym 3 tape `shouldBe` ' '
            getSym 4 tape `shouldBe` ' '
            getSym 5 tape `shouldBe` ' '

setSymSpec :: Spec
setSymSpec = do
    describe "setSym" $ do
        it "sets the tape symbol" $ do
            let tape  = fromString "abc"
                tape' = setSym 1 'x' tape
            tape' `shouldRead` "axc"

        it "does not set symbol other than that specified" $ do
            let tape = fromString "abc"
                tape' = setSym 1 'x' tape
            tape' `shouldRead` "axc"
