module State.MachineSpec (machineSpec) where

import State.Machine
import Test.Hspec

machineSpec :: Spec
machineSpec = do
    describe "Machine" $ do
        fmapSpec
        appSpec
        bindSpec

fmapSpec :: Spec
fmapSpec = do
    describe "fmap" $ do
        it "has no effect if the machine accepted" $ do
            fmap (+1) HaltA `shouldBe` HaltA

        it "has no effect if the machine rejected" $ do
            fmap (+1) HaltR `shouldBe` HaltR

        it "performs the function if the machine is running" $ do
            fmap (+1) (Inter 2) `shouldBe` Inter 3

appSpec :: Spec
appSpec = do
    describe "app" $ do
        it "has no effect if the machine accepted" $ do
            Inter (+1) <*> HaltA `shouldBe` HaltA

        it "has no effect if the machine rejected" $ do
            Inter (+1) <*> HaltR `shouldBe` HaltR

        it "performs the function if the machine is running" $ do
            Inter (+1) <*> Inter 2 `shouldBe` Inter 3

bindSpec :: Spec
bindSpec = do
    describe "bind" $ do
        it "has no effect if the machine accepted" $ do
            (HaltA >>= return . (+1)) `shouldBe` HaltA

        it "has no effect if the machine rejected" $ do
            (HaltR >>= return . (+1)) `shouldBe` HaltR

        it "performs the function if the machine is running" $ do
            (Inter 2 >>= return . (+1)) `shouldBe` Inter 3
