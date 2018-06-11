module Syntax.Variable where

import Syntax.Common
import Syntax.Identifier

import Debug.Trace

-- Attempts to parse an identifier used to declare a new variable.
-- Fails if the variable already exists. If the variable does not exist
-- it is added to the environment.
newVarId :: ParserM VarName
newVarId = newId snakeId

-- Parses a variable and its type. Fails if the variable does not already
-- exist in the environment (which is determined by getType).
refVarIdWith :: (Identifier -> ParserM EnvDecl) -> ParserM (VarName, DataType)
refVarIdWith getType = do
    (name, idType) <- refIdWith snakeId getType
    case idType of
        PVar varType -> return (name, varType)
        _            -> fail "Expected variable"

-- Attempts to use a declared variable. If the variable does not exist, or the
-- types do not match, then parsing fails.
expTypeVarId :: DataType -> ParserM VarName
expTypeVarId expType = expTypeId snakeId (PVar expType)

tapeSymbol :: ParserM TapeSymbol
tapeSymbol = noneOf "\'\""

symExpr :: ParserM SymExpr
symExpr = Read <$ lTok "read" <* lWhitespace <*> tapeExpr
      <|> SymLit <$> between (char '\'') (lTok "\'") tapeSymbol
      <|> SymVar <$> expTypeVarId SymType

tapeExpr :: ParserM TapeExpr
tapeExpr = TapeLit <$> quoted (many tapeSymbol)
       <|> TapeVar <$> expTypeVarId TapeType

anyValExpr :: ParserM AnyValExpr
anyValExpr = try (S <$> symExpr)
         <|> T <$> tapeExpr

expTypeExpr :: DataType -> ParserM AnyValExpr
expTypeExpr SymType  = S <$> symExpr
expTypeExpr TapeType = T <$> tapeExpr

varDecl :: ParserM Stm
varDecl = do
    _ <- lTok "let"
    name <- newVarId
    _ <- lTok "="
    v <- anyValExpr

    putM name (PVar (typeOf v))

    return (VarDecl name v)

-- Type of data passed to a function, the EBNF of which is:
--  TypeAnnotation: 'Tape' | 'Sym' | StructName
typeAnnotation :: ParserM DataType
typeAnnotation = SymType <$ lTok "Sym"
             <|> TapeType <$ lTok "Tape"

typeAnnotated :: ParserM Identifier -> ParserM (Identifier, DataType)
typeAnnotated p = do
    name <- p
    _ <- lTok ":"
    idType <- typeAnnotation
    return (name, idType)
