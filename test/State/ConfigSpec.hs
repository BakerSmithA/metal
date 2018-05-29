module State.ConfigSpec (configSpec) where

import State.Config
import Syntax.Tree
import Test.Hspec
import Data.Map as Map
import Data.Maybe

configSpec :: Spec
configSpec = do
    describe "Config" $ do
        leftSpec
        rightSpec
        getCurrSpec
        setCurrSpec
        varSpec
        funcSpec

pos :: TapeName -> Maybe Config -> Maybe Integer
pos tapeName config = do
    c <- config
    (_, p) <- Map.lookup tapeName (tapes c)
    return p

leftSpec :: Spec
leftSpec = do
    describe "left" $ do
        it "does not move the read-write head if it is zeroed" $ do
            pos "tape" (left "tape" (initial "tape")) `shouldBe` Just 0

        it "moves the read-write head left if it is not zeroed" $ do
            let config = (right "tape" (initial "tape")) >>= \c1 -> right "tape" c1 >>= \c2 -> left "tape" c2
            pos "tape" config `shouldBe` Just 1

        it "fails if the tape does not exist" $ do
            pos "tape" (left "nothing" (initial "tape")) `shouldBe` Nothing

rightSpec :: Spec
rightSpec = do
    describe "right" $ do
        it "moves the read-write head right" $ do
            pos "tape" (right "tape" (initial "tape")) `shouldBe` Just 1

        it "fails if the tape does not exist" $ do
            pos "tape" (right "nothing" (initial "tape")) `shouldBe` Nothing

getCurrSpec :: Spec
getCurrSpec = do
    describe "getCurr" $ do
        let config = "tape" `fromString` "abc"

        it "returns the symbol under the read-write head on the tape" $ do
            getCurr "tape" (config)                         `shouldBe` Just 'a'
            getCurr "tape" (fromJust $ right "tape" config) `shouldBe` Just 'b'

        it "fails if the tape does not exist" $ do
            getCurr "nothing" config `shouldBe` Nothing

setCurrSpec :: Spec
setCurrSpec = do
    describe "setCurr" $ do
        it "sets the symbol under the read-write head" $ do
            let config0 = fromString "tape" "abc"
                config1 = fromJust $ setCurr "tape" '1' config0
                config2 = fromJust $ left "tape" (fromJust (setCurr "tape" '2' config1))

            getCurr "tape" config1 `shouldBe` Just '1'
            getCurr "tape" config2 `shouldBe` Just '2'

        it "fails if the tape does not exist" $ do
            let config = "tape" `fromString` "abc"
            setCurr "nothing" 'x' config `shouldBe` Nothing

varSpec :: Spec
varSpec = do
    describe "variable environment" $ do
        it "returns Nothing if the variable is undefined" $ do
            lookupVar "x" (initial "tape") `shouldBe` Nothing

        it "allows variables to be added and retrieved" $ do
            let env = addVar "x" '1' (initial "tape")
            lookupVar "x" env `shouldBe` Just '1'

        it "overrides previous variable declarations" $ do
            let env  = addVar "x" '1' (initial "tape")
                env' = addVar "x" '2' env
            lookupVar "x" env' `shouldBe` Just '2'

funcSpec :: Spec
funcSpec = do
    describe "function environment" $ do
        it "returns Nothing if the function is undefined" $ do
            lookupFunc "f" (initial "tape") `shouldBe` Nothing

        it "allows functions to be added and retrieved" $ do
            let env = addFunc "f" [] (MoveRight "tape") (initial "tape")
            lookupFunc "f" env `shouldBe` Just ([], (MoveRight "tape"))

        it "allows functions with arguments to be added and retrieved" $ do
            let env = addFunc "f" ["a", "b"] (MoveRight "tape") (initial "tape")
            lookupFunc "f" env `shouldBe` Just (["a", "b"], (MoveRight "tape"))

        it "overrides previous function declarations" $ do
            let env  = addFunc "f" [] (MoveRight "tape") (initial "tape")
                env' = addFunc "f" [] (MoveLeft "tape") env
            lookupFunc "f" env' `shouldBe` Just ([], (MoveLeft "tape"))
