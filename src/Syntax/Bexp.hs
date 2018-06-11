module Syntax.Bexp where

import Syntax.Common
import Syntax.Variable
import Text.Megaparsec.Expr

-- Parses the basis elements of the boolean expressions, plus boolean
-- expressions wrapped in parenthesis.
bexp' :: ParserM Bexp
bexp' = try (parens bexp)
    <|> TRUE  <$ lTok "True"
    <|> FALSE <$ lTok "False"
    <|> try (Eq <$> p <* lTok "==" <*> p)
    <|> try (Le <$> p <* lTok "<=" <*> p)
    <|> try (Ne <$> p <* lTok "!=" <*> p) where
        p = symExpr

-- The operators that can work on boolean expressions. There is no precedence,
-- instead the expression is evaualted from left to right.
bexpOps :: [[Operator ParserM Bexp]]
bexpOps = [[Prefix (Not <$ lTok "not")],
           [InfixL (And <$ lTok "and"), InfixL (Or <$ lTok "or")]]

-- Parses a boolean expression, allowing for parenthesis to specify the
-- intended parse. The EBNF syntax of boolean expressions is:
--  Bexp : 'True'
--       | 'False'
--       | 'not' Bexp
--       | Bexp 'and' Bexp
--       | Bexp 'or' Bexp
--       | DerivedValue '==' DerivedValue
--       | DerivedValue '<=' DerivedValue
bexp :: ParserM Bexp
bexp = makeExprParser bexp' bexpOps
