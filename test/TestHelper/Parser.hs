module TestHelper.Parser where

import Syntax.Tree
import Text.Megaparsec
import Test.Hspec
import Test.HUnit.Lang

-- Asserts that the parse of a program should contain the given statement.
shouldParseStm :: (Show t, Show e) => Either (ParseError t e) Stm -> Stm -> Expectation
shouldParseStm result stm = either handleErr success result where
    handleErr err  = assertFailure ("Expected program, got error: " ++ (show err))
    success body = body `shouldBe` stm
