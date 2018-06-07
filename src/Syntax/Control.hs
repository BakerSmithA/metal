module Syntax.Control where

import Syntax.Helper
import Syntax.Bexp

-- Parses if-else statement, the EBNF syntax of which is:
--  If     : 'if' { Stm } ElseIf
--  ElseIf : 'else if' { Stm } ElseIf | Else
--  Else   : 'else' { Stm } | ε
ifStm :: Parser Stm -> Parser Stm
ifStm stm = If <$ lTok "if" <*> bexp <*> block (braces stm) <*> many elseIfClause <*> elseClause where
    elseIfClause = do
        bool <- lTok "else if" *> bexp
        statement <- block (braces stm)
        return (bool, statement)
    elseClause = optional (lTok "else" *> block (braces stm))

whileStm :: Parser Stm -> Parser Stm
whileStm stm = While <$ lTok "while" <*> bexp <*> block (braces stm)
