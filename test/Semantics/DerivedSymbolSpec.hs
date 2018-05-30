module Semantics.DerivedSymbolSpec where

import State.Config as Config
import State.Error
import Syntax.Tree
import TestHelper.Config
import TestHelper.Denotational
import Test.Hspec hiding (shouldReturn, shouldContain)

derivedSymbolValSpec :: Spec
derivedSymbolValSpec = do
    let testConfig  = right (Config.fromString "tape" "abc")
        testConfig' = putVar "x" '1' testConfig

    describe "derivedSymbolVal" $ do
        it "reads the symbol under the read-write head" $ do
            let result = evalDerivedSymbol (Read "tape") testConfig'
            result `shouldReturn` 'b'

        it "returns the literal" $ do
            let result = evalDerivedSymbol (Literal 'm') testConfig'
            result `shouldReturn` 'm'

        it "returns the value of a variable" $ do
            let result = evalDerivedSymbol (Var "x") testConfig'
            result `shouldReturn` '1'

        it "fails if the variable is not defined" $ do
            let result = evalDerivedSymbol (Var "undef") testConfig'
            result `shouldThrow` (== UndefVar "undef")
