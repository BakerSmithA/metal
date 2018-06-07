module Syntax.Helper
( module Syntax.Helper
, module Text.Megaparsec
, module Control.Monad.State.Lazy
, module Syntax.ParseState
, module Syntax.Tree
) where

import Control.Monad (void)
import Control.Monad.State.Lazy (StateT, when, modify, get, put, lift, runStateT, liftM)
import qualified Text.Megaparsec.String as M
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec hiding (State)
import qualified Syntax.Env as E
import Syntax.ParseState
import Syntax.Tree

type Parser = StateT ParseState M.Parser
-- Retrieve the variable or function environment from the parse state.
type GetEnv a = ParseState -> E.Env a
-- Modify the environment in a parse state.
type ModifyEnv a = (E.Env a -> E.Env a) -> ParseState -> ParseState

-- Produces a whitespace consumer using `sc` as the space consumer. Consumes
-- whole line and in-line comments. The syntax for both comment types are the
-- same as C, with '//' indicating a whole line comment and '/* ... */'
-- indicating an in-line comment.
whitespaceConsumer :: M.Parser Char -> M.Parser ()
whitespaceConsumer sc = L.space (void sc) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//" <* void (many newline)
        blockCmnt = L.skipBlockComment "/*" "*/" <* void (many newline)

-- Comsumes whitespace and comments, but not newlines.
whitespace :: M.Parser ()
whitespace = whitespaceConsumer (oneOf "\t ")

lWhitespace :: Parser ()
lWhitespace = lift whitespace

whitespaceNewline :: M.Parser ()
whitespaceNewline = whitespaceConsumer spaceChar

-- Consumes whitespace, comments, and newlines.
lWhitespaceNewline :: Parser ()
lWhitespaceNewline = lift whitespaceNewline

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
tok :: String -> M.Parser String
tok s = string s <* whitespace

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
lTok :: String -> Parser String
lTok = lift . tok

-- Parses a string enclosed in parenthesis.
parens :: Parser a -> Parser a
parens = between (lTok "(") (lTok ")")

-- Parses a string enclosed in curly braces.
braces :: Parser a -> Parser a
braces = between (lTok "{" <* lWhitespaceNewline) (lTok "}")

-- Parses a string enclosed in curly braces, but also allows any variables or
-- functions inside the block to overwrite definitions outside the block.
block :: Parser a -> Parser a
block p = do
    savedState <- get
    modify (modifyVarEnv E.descendScope)
    modify (modifyFuncEnv E.descendScope)

    x <- p

    put savedState
    return x

quoted :: Parser a -> Parser a
quoted = between (lTok "\"") (lTok "\"")

-- Parses a string encased in double quotes.
quotedString :: Parser String
quotedString = quoted (many (noneOf "\""))
