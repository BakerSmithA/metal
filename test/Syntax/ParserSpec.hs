module Syntax.ParserSpec where

import Test.Hspec

parserSpec :: Spec
parserSpec = do
    describe "Tests" $ do
      it "Testing test" $ do
       True `shouldBe` True
