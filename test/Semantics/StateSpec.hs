module Semantics.StateSpec where

import Semantics.State
import Test.Hspec

testFunc :: String -> Int
testFunc "x" = 1
testFunc "y" = 2
testFunc  _  = 3

stateSpec :: Spec
stateSpec = do
    updateSpec
    updateManySpec
    placeOnTapeSpec

updateSpec :: Spec
updateSpec = describe "update" $ do
    let f' = update "x" 4 testFunc

    it "updates the value returned by a function" $ do
        f' "x" `shouldBe` 4

    it "does not modify the value of other inputs" $ do
        f' "y" `shouldBe` 2
        f' "z" `shouldBe` 3

updateManySpec :: Spec
updateManySpec = describe "updateMany" $ do
    let f' = updateMany [("x", 4), ("y", 5)] testFunc

    it "updates the values returned by a function" $ do
        f' "x" `shouldBe` 4
        f' "y" `shouldBe` 5

    it "does not modify the value of other inputs" $ do
        f' "z" `shouldBe` 3

placeOnTapeSpec :: Spec
placeOnTapeSpec = describe "placeOnTape" $ do
    let tape = placeOnTape "abc" initialTape

    it "places a string at the start of a tape" $ do
        tape 0 `shouldBe` 'a'
        tape 1 `shouldBe` 'b'
        tape 2 `shouldBe` 'c'

    it "does not add any symbols past the end of the string" $ do
        tape 4 `shouldBe` ' '
