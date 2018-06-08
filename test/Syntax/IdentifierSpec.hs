module Syntax.IdentifierSpec where

import Syntax.Tree
import Syntax.ParseState as S
import Syntax.Bexp
import Syntax.Variable
import Syntax.Parser
import Syntax.Common
import Syntax.Identifier
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelper.Parser
import Text.Megaparsec (parse)

identifierSpec :: Spec
identifierSpec = do
    snakeIdSpec

snakeIdSpec :: Spec
snakeIdSpec = do
    describe "snakeIdSpec" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState snakeId "" "vname" `shouldParse` "vname"

        it "parses names containing underscores characters" $ do
            parseEmptyState snakeId "" "v_name" `shouldParse` "v_name"

        it "parses names containing digits" $ do
            parseEmptyState snakeId "" "v1234" `shouldParse` "v1234"

        it "parses if the variable name is a superset of a reserved word" $ do
            parseEmptyState snakeId "" "true_v" `shouldParse` "true_v"

        it "parses underscores at the start" $ do
            parseEmptyState snakeId "" "_name" `shouldParse` "_name"

        it "fails if contains uppercase" $ do
            parseEmptyState snakeId "" `shouldFailOn` "Vname"

        it "fails if the start character is a digit" $ do
            parseEmptyState snakeId "" `shouldFailOn` "1_"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState snakeId "" `shouldFailOn` "True"
