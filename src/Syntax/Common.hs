module Syntax.Common
( module Syntax.Common
, module Text.Megaparsec
, module Syntax.ParseState
, module Syntax.Tree
) where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Lazy (put, get)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec hiding (State)
import Syntax.ParseState
import Syntax.Tree

-- Produces a whitespace consumer using `sc` as the space consumer. Consumes
-- whole line and in-line comments. The syntax for both comment types are the
-- same as C, with '//' indicating a whole line comment and '/* ... */'
-- indicating an in-line comment.
whitespaceConsumer :: Parser Char -> Parser ()
whitespaceConsumer sc = L.space (void sc) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//" <* void (many newline)
        blockCmnt = L.skipBlockComment "/*" "*/" <* void (many newline)

-- Comsumes whitespace and comments, but not newlines.
whitespace :: Parser ()
whitespace = whitespaceConsumer (oneOf "\t ")

lWhitespace :: ParserM ()
lWhitespace = lift whitespace

whitespaceNewline :: Parser ()
whitespaceNewline = whitespaceConsumer spaceChar

-- Consumes whitespace, comments, and newlines.
lWhitespaceNewline :: ParserM ()
lWhitespaceNewline = lift whitespaceNewline

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
tok :: String -> Parser String
tok s = string s <* whitespace

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
lTok :: String -> ParserM String
lTok = lift . tok

-- Parses a string enclosed in parenthesis.
parens :: ParserM a -> ParserM a
parens = between (lTok "(") (lTok ")")

-- Parses a string enclosed in curly braces.
braces :: ParserM a -> ParserM a
braces = between (lTok "{" <* lWhitespaceNewline) (lTok "}")

-- Allows any variables or functions inside the block to overwrite definitions
-- outside the block.
block :: ParserM a -> ParserM a
block p = do
    savedState <- get
    descendScopeM
    x <- p
    put savedState
    return x

quoted :: ParserM a -> ParserM a
quoted = between (lTok "\"") (lTok "\"")

-- Parses a string encased in double quotes.
quotedString :: ParserM String
quotedString = quoted (many (noneOf "\""))

-- Creates a parser for each member of expTypes. Useful for ensuring many
-- parses have the correct type, e.g. when invoking a function.
matchedTypes :: (DataType -> ParserM a) -> [DataType] -> ParserM [a]
matchedTypes makeP expTypes = foldl combine (return []) ps where
    combine acc p = (++) <$> acc <*> (fmap (\x -> [x]) p)
    ps = zipWith (\x y -> x y) (repeat makeP) expTypes
