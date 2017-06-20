module State.TapeSpec (tapeSpec) where

import State.Tape
import Test.Hspec

tapeSpec :: Spec
tapeSpec = do
    fromStringSpec
    setSymSpec

fromStringSpec :: Spec
fromStringSpec = do
    describe "fromString" $ do
        it "places a string at the start of the tape" $ do
            let tape = fromString "abc"
            getSym 0 tape `shouldBe` 'a'
            getSym 1 tape `shouldBe` 'b'
            getSym 2 tape `shouldBe` 'c'

        it "leaves the rest of the tape as space characters" $ do
            let tape = fromString "abc"
            getSym 3 tape `shouldBe` ' '

setSymSpec :: Spec
setSymSpec = do
    describe "setSym" $ do
        it "sets the tape symbol" $ do
            let tape  = fromString "abc"
                tape' = setSym 1 'x' tape
            getSym 1 tape' `shouldBe` 'x'

        it "does not set symbol other than that specified" $ do
            let tape = fromString "abc"
                tape' = setSym 1 'x' tape

            getSym 0 tape' `shouldBe` 'a'
            getSym 2 tape' `shouldBe` 'c'
            getSym 3 tape' `shouldBe` ' '
