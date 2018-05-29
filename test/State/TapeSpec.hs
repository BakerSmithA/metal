module State.TapeSpec (tapeSpec) where

import State.Tape
import Syntax.Tree
import Test.Hspec

-- Asserts that the tape should contain `syms` at its start.
shouldRead :: Tape -> [TapeSymbol] -> Expectation
shouldRead tape syms = contents (fromString syms) `shouldBe` contents tape

tapeSpec :: Spec
tapeSpec = do
    describe "Tape" $ do
        fromStringSpec
        leftSpec
        rightSpec
        getSymSpec
        setSymSpec

fromStringSpec :: Spec
fromStringSpec = do
    describe "fromString" $ do
        it "places a string at the start of the tape" $ do
            let tape = fromString "abc"
            tape `shouldRead` "abc"

leftSpec :: Spec
leftSpec = do
    describe "left" $ do
        it "does nothing if already zeroed" $ do
            let tape = fromString "abc"
            pos (left tape) `shouldBe` 0

        it "moves left" $ do
            let tape = right $ right $ fromString "abc"
            pos (left tape) `shouldBe` 1

rightSpec :: Spec
rightSpec = do
    describe "right" $ do
        it "moves right" $ do
            let tape = fromString "abc"
            pos (right tape) `shouldBe` 1

getSymSpec :: Spec
getSymSpec = do
    describe "getSym" $ do
        it "returns the symbol at the specified position on the tape" $ do
            let tape = fromString "abc"
            getSym tape `shouldBe` 'a'
            getSym (right tape) `shouldBe` 'b'
            getSym (right (right tape)) `shouldBe` 'c'

        it "returns the blank (space) character if the cell is empty" $ do
            let tape = fromString "abc"
            getSym (right (right (right tape))) `shouldBe` ' '

setSymSpec :: Spec
setSymSpec = do
    describe "setSym" $ do
        it "sets the tape symbol" $ do
            let tape  = fromString "abc"
                tape' = setSym 'x' (right tape)
            tape' `shouldRead` "axc"

        it "does not set symbol other than that specified" $ do
            let tape = fromString "abc"
                tape' = setSym 'x' (right tape)
            tape' `shouldRead` "axc"
