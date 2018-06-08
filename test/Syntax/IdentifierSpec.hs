module Syntax.IdentifierSpec where

import Syntax.Parser
import Syntax.Identifier
import Test.Hspec
import Test.Hspec.Megaparsec

identifierSpec :: Spec
identifierSpec = do
    snakeIdSpec
    camelIdSpec

snakeIdSpec :: Spec
snakeIdSpec = do
    describe "snakeId" $ do
        it "parses names beginning with a lower char" $ do
            parseEmptyState snakeId "" "vname" `shouldParse` "vname"

        it "parses names containing underscores characters" $ do
            parseEmptyState snakeId "" "v_name" `shouldParse` "v_name"

        it "parses names containing digits" $ do
            parseEmptyState snakeId "" "v1234" `shouldParse` "v1234"

        it "parses if the variable name is a superset of a reserved word" $ do
            parseEmptyState snakeId "" "left_v" `shouldParse` "left_v"

        it "parses underscores at the start" $ do
            parseEmptyState snakeId "" "_name" `shouldParse` "_name"

        it "fails if contains uppercase" $ do
            parseEmptyState snakeId "" `shouldFailOn` "Vname"

        it "fails if the start character is a digit" $ do
            parseEmptyState snakeId "" `shouldFailOn` "1_"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState snakeId "" `shouldFailOn` "left"

camelIdSpec :: Spec
camelIdSpec = do
    describe "camelId" $ do
        it "parses names with an upper chars" $ do
            parseEmptyState camelId "" "NAME" `shouldParse` "NAME"

        it "parses names containing lowercase characters" $ do
            parseEmptyState camelId "" "NameName" `shouldParse` "NameName"

        it "parses names containing digits" $ do
            parseEmptyState camelId "" "Name1" `shouldParse` "Name1"

        it "parses if the variable name is a superset of a reserved word" $ do
            parseEmptyState camelId "" "TrueV" `shouldParse` "TrueV"

        it "fails if underscore at the start" $ do
            parseEmptyState camelId "" `shouldFailOn` "_name"

        it "fails if lower case at start" $ do
            parseEmptyState camelId "" `shouldFailOn` "name"

        it "fails if the start character is a digit" $ do
            parseEmptyState camelId "" `shouldFailOn` "1Name"

        it "fails if the function name is a reserved keyword" $ do
            parseEmptyState camelId "" `shouldFailOn` "True"
