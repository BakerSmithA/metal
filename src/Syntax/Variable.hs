module Syntax.Variable where

import Syntax.Common
import Syntax.Identifier

-- Attempts to parse an identifier used to declare a new variable.
-- Fails if the variable already exists. If the variable does not exist
-- it is added to the environment.
newVarId :: ParserM VarName
newVarId = newId snakeId

-- Parses a variable and its type. Fails if the variable does not already
-- exist in the environment.
refVarId :: ParserM (VarName, DataType)
refVarId = do
    (name, idType) <- refId snakeId
    case idType of
        PVar varType -> return (name, varType)
        _            -> fail "Expected variable"

tapeSymbol :: ParserM TapeSymbol
tapeSymbol = noneOf "\'\""

symExpr :: ParserM SymExpr
symExpr = Read <$ lTok "read" <* lWhitespace <*> tapeExpr
      <|> SymLit <$> between (char '\'') (lTok "\'") tapeSymbol
      <|> SymVar <$> expVarId SymType

tapeExpr :: ParserM TapeExpr
tapeExpr = TapeLit <$> quoted (many tapeSymbol)
       <|> TapeVar <$> expVarId TapeType

anyValExpr :: ParserM AnyValExpr
anyValExpr = try (S <$> symExpr)
         <|> T <$> tapeExpr

expVarId :: DataType -> ParserM VarName
expVarId t = expTypeId refVarId (==t)

expAnyValExpr :: DataType -> ParserM AnyValExpr
expAnyValExpr = expDataType anyValExpr

varDecl :: ParserM Stm
varDecl = do
    _ <- lTok "let"
    name <- newVarId
    _ <- lTok "="
    v <- anyValExpr

    putM name (PVar (typeOf v))

    return (VarDecl name v)

-- Type of data (passed to a function or as part of a struct member decl). EBNF:
--  TypeAnnotation: 'Tape' | 'Sym' | StructName
typeAnnotation :: ParserM DataType
typeAnnotation = SymType <$ lTok "Sym"
             <|> TapeType <$ lTok "Tape"
             <|> CustomType <$> fmap fst (refId camelId)

-- Type of data (passed to a function or as part of a struct member decl).
-- Allows the name of a being declared to be used in member declarations.
--  TypeAnnotation: 'Tape' | 'Sym' | StructName
recTypeAnnotation :: StructName -> ParserM DataType
recTypeAnnotation name = try typeAnnotation
                     <|> CustomType <$> lTok name

-- Parses a variable name with a type annotation, e.g. 'x:Tape'.
typeAnnotatedWith :: ParserM DataType -> ParserM Identifier -> ParserM (Identifier, DataType)
typeAnnotatedWith dataType p = do
    name <- p
    _ <- lTok ":"
    idType <- dataType
    return (name, idType)

-- Parses a variable name with a type annotation, e.g. 'x:Tape'.
typeAnnotated :: ParserM Identifier -> ParserM (Identifier, DataType)
typeAnnotated = typeAnnotatedWith typeAnnotation

-- Parses a variable name with a recursive type annotation, e.g. allows for
-- 'struct S { x:S }'
recTypeAnnotated :: StructName -> ParserM Identifier -> ParserM (Identifier, DataType)
recTypeAnnotated structName = typeAnnotatedWith (recTypeAnnotation structName)
