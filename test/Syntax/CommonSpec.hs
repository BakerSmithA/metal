module Syntax.CommonSpec where

import Syntax.Parser
import Syntax.Common
import Test.Hspec
import Test.Hspec.Megaparsec

commonSpec :: Spec
commonSpec = do
    encasedStringSpec

encasedStringSpec :: Spec
encasedStringSpec = do
    describe "quotedString" $ do
        it "parses the empty string" $ do
            parseEmptyState quotedString "" "\"\"" `shouldParse` ""

        it "parses a string enclosed in double quotes" $ do
            parseEmptyState quotedString "" "\"str\"" `shouldParse` "str"

        it "parses strings enclosed in double quotes that contain spaces" $ do
            parseEmptyState quotedString "" "\"This is a string\"" `shouldParse` "This is a string"

        it "fails to parse if a starting quote is missing" $ do
            parseEmptyState quotedString "" `shouldFailOn` "str\""

        it "fails to parse if an ending quote is missing" $ do
            parseEmptyState quotedString "" `shouldFailOn` "\"str"
