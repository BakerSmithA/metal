module State.TreeSpec (treeSpec) where

import State.Tree
import Test.Hspec
import Control.Monad.Identity

testTree :: Integer -> Identity ([Integer], String)
testTree 0 = return ([1, 2], "0")
testTree 1 = return ([3], "1")
testTree 2 = return ([], "2")
testTree 3 = return ([], "3")
testTree _ = error "No node"

loopTree1 :: Integer -> Identity ([Integer], String)
loopTree1 0 = return ([1], "0")
loopTree1 1 = return ([0], "1")
loopTree1 _ = error "No node"

loopTree2 :: Integer -> Identity ([Integer], String)
loopTree2 0 = return ([1], "0")
loopTree2 1 = return ([0, 2, 3], "1")
loopTree2 2 = return ([3], "2")
loopTree2 3 = return ([2], "3")
loopTree2 _ = error "No node"

treeSpec :: Spec
treeSpec = do
    describe "Tree" $ do
        dfFlattenTreeSpec

dfFlattenTreeSpec :: Spec
dfFlattenTreeSpec = do
    describe "dfFlattenTree" $ do
        it "flattens the tree using DFS" $ do
            let expected = return ["3", "1", "2", "0"]
            dfFlattenTree testTree [0] `shouldBe` expected

        it "does not include data in loops more than once" $ do
            let expected = return ["1", "0"]
            dfFlattenTree loopTree1 [0] `shouldBe` expected

        it "visits braches of loops" $ do
            let expected = return ["3", "2", "1", "0"]
            dfFlattenTree loopTree2 [0] `shouldBe` expected
