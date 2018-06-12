module Syntax.VariableExpr where

import Syntax.Common
import Syntax.Identifier
import Syntax.Struct

tapeSymbol :: ParserM TapeSymbol
tapeSymbol = noneOf "\'\""

symExpr :: ParserM SymExpr
symExpr = Read <$ lTok "read" <* lWhitespace <*> tapeExpr
      <|> SymLit <$> between (char '\'') (lTok "\'") tapeSymbol
      <|> SymVar <$> expVarId SymType

tapeExpr :: ParserM TapeExpr
tapeExpr = TapeLit <$> quoted (many tapeSymbol)
       <|> TapeVar <$> expVarId TapeType

objExpr :: ParserM ObjExpr
objExpr = makeObj <|> objVar where
    makeObj = do
        (name, ms) <- refStruct
        args <- matchedTypes (maybeParens . expAnyValExpr) (map memberVarType ms)
        return (NewObj name args)
    objVar = do
        (name, (CustomType structName)) <- expType refVarId (isCustomType . snd)
        return (ObjVar structName name)

anyValExpr :: ParserM AnyValExpr
anyValExpr = try (S <$> symExpr)
         <|> try (T <$> tapeExpr)
         <|> try (C <$> objExpr)

expVarId :: DataType -> ParserM VarName
expVarId t = expTypeId refVarId (==t)

expAnyValExpr :: DataType -> ParserM AnyValExpr
expAnyValExpr = expDataType anyValExpr

-- Parses a variable (e.g. tape, symbol, object) declaration, EBNF:
--  'let' VarName '=' AnyTypeExpr
varDecl :: ParserM Stm
varDecl = do
    _ <- lTok "let"
    name <- newVarId
    _ <- lTok "="
    v <- anyValExpr

    putM name (PVar (typeOf v))

    return (VarDecl name v)
