module Syntax.Variable where

import Syntax.Common
import Syntax.Identifier

-- Attempts to parse an identifier used to declare a new variable.
-- Fails if the variable already exists. If the variable does not exist
-- it is added to the environment. EBNF:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
newVar :: DataType -> Parser VarName
newVar varType = putNewId snakeId varType varEnv modifyVarEnv

-- Attempts to use a declared variable. If the variable does not exist, or the
-- types do not match, then parsing fails. EBNF:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
refVar :: DataType -> Parser VarName
refVar expectedType = refExpTypedId snakeId expectedType varEnv

-- Parses a derived symbol, the EBNF syntax of which is:
--  DerivedValue : 'read'
--                | VarName
--                | \' TapeSymbol \'
derivedSymbol :: DataType -> Parser DerivedValue
derivedSymbol expectedVarType = Read <$ lTok "read" <* lWhitespace <*> refVar TapeType
                            <|> Var <$> refVar expectedVarType
                            <|> Literal <$> between (char '\'') (lTok "\'") tapeSymbol
                            <|> parens (derivedSymbol expectedVarType)

-- Parses a tape symbol, the EBNF syntax of which is:
--  TapeSymbol  : LowerChar | UpperChar | Digit | ASCII-Symbol
tapeSymbol :: Parser TapeSymbol
tapeSymbol = noneOf "\'\""

-- Parsers the contents of a tape literal, e.g. "abcd"
tapeLiteral :: Parser String
tapeLiteral = quoted (many tapeSymbol)

-- Type of data passed to a function, the EBNF of which is:
--  FuncArgType : 'Tape' | 'Sym'
dataType :: Parser DataType
dataType = SymType <$ lTok "Sym"
       <|> TapeType <$ lTok "Tape"

-- Variable which has a type, the EBNF of which is:
--  TypedVar : VarName ':' Type
typedVar :: Parser Identifier -> Parser (Identifier, DataType)
typedVar p = do
    name <- p
    _ <- lTok ":"
    argType <- dataType
    return (name, argType)
