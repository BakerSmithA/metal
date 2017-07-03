module Semantics.DerivedSymbolSpec where

import State.Config as Config
import State.Error
import State.Tape()
import Syntax.Tree
import TestHelper.Denotational
import Test.Hspec hiding (shouldContain, shouldThrow)

derivedSymbolValSpec :: Spec
derivedSymbolValSpec = do
    let testConfig  = right (Config.fromString "abc")
        testConfig' = addVar "x" '1' testConfig

    describe "derivedSymbolVal" $ do
        it "reads the symbol under the read-write head" $ do
            let result = evalDerivedSymbol Read testConfig'
            result `shouldContain` 'b'

        it "returns the literal" $ do
            let result = evalDerivedSymbol (Literal 'm') testConfig'
            result `shouldContain` 'm'

        it "returns the value of a variable" $ do
            let result = evalDerivedSymbol (Var "x") testConfig'
            result `shouldContain` '1'

        it "fails if the variable is not defined" $ do
            let result = evalDerivedSymbol (Var "undef") testConfig'
            result `shouldThrow` (UndefVar "undef")
