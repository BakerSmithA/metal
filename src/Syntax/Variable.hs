module Syntax.Variable where

import Syntax.Common
import Syntax.Identifier

-- Attempts to parse an identifier used to declare a new variable.
-- Fails if the variable already exists. If the variable does not exist
-- it is added to the environment. EBNF:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
newVar :: DataType -> ParserM VarName
newVar varType = putNewId snakeId (PVar varType)

-- Attempts to use a declared variable. If the variable does not exist, or the
-- types do not match, then parsing fails. EBNF:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
refVar :: DataType -> ParserM VarName
refVar expType = refExpTypeId snakeId (PVar expType)

-- Parses a derived symbol, the EBNF syntax of which is:
--  DerivedValue : 'read'
--                | VarName
--                | \' TapeSymbol \'
derivedSymbol :: DataType -> ParserM DerivedValue
derivedSymbol expectedVarType = Read <$ lTok "read" <* lWhitespace <*> refVar TapeType
                            <|> Var <$> refVar expectedVarType
                            <|> Literal <$> between (char '\'') (lTok "\'") tapeSymbol
                            <|> parens (derivedSymbol expectedVarType)

-- Parses a tape symbol, the EBNF syntax of which is:
--  TapeSymbol  : LowerChar | UpperChar | Digit | ASCII-Symbol
tapeSymbol :: ParserM TapeSymbol
tapeSymbol = noneOf "\'\""

-- ParserMs the contents of a tape literal, e.g. "abcd"
tapeLiteral :: ParserM String
tapeLiteral = quoted (many tapeSymbol)

-- Type of data passed to a function, the EBNF of which is:
--  FuncArgType : 'Tape' | 'Sym'
dataType :: ParserM DataType
dataType = SymType <$ lTok "Sym"
       <|> TapeType <$ lTok "Tape"

-- Variable which has a type, the EBNF of which is:
--  TypedVar : VarName ':' Type
typedVar :: ParserM Identifier -> ParserM (Identifier, DataType)
typedVar p = do
    name <- p
    _ <- lTok ":"
    argType <- dataType
    return (name, argType)
