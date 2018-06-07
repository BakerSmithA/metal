module Syntax.Variable where

import Syntax.Helper
import Syntax.Identifier

-- Parses a derived symbol, the EBNF syntax of which is:
--  DerivedValue : 'read'
--                | VarName
--                | \' TapeSymbol \'
derivedSymbol :: DataType -> Parser DerivedValue
derivedSymbol expectedVarType = Read <$ lTok "read" <* lWhitespace <*> refVar TapeType
                            <|> Var <$> refVar expectedVarType
                            <|> Literal <$> between (char '\'') (lTok "\'") tapeSymbol
                            <|> parens (derivedSymbol expectedVarType)

-- Parsers the contents of a tape literal, e.g. "abcd"
tapeLiteral :: Parser String
tapeLiteral = quoted (many tapeSymbol)

-- Type of data passed to a function, the EBNF of which is:
--  FuncArgType   : 'Tape' | 'Sym'
dataType :: Parser DataType
dataType = SymType <$ lTok "Sym"
       <|> TapeType <$ lTok "Tape"
