module Syntax.Variable where

import Syntax.Common
import Syntax.Identifier

-- Attempts to parse an identifier used to declare a new variable.
-- Fails if the variable already exists. If the variable does not exist
-- it is added to the environment.
newVarId :: ParserM VarName
newVarId = newId snakeId

-- Parses a variable, returning its name and type.
refVarId :: ParserM (VarName, DataType)
refVarId = do
    (name, idType) <- refId snakeId
    case idType of
        PVar t -> return (name, t)
        _      -> fail "Expected variable"

-- Attempts to use a declared variable. If the variable does not exist, or the
-- types do not match, then parsing fails.
expTypeVarId :: DataType -> ParserM VarName
expTypeVarId expType = expTypeId snakeId (PVar expType)

-- Type of data passed to a function, the EBNF of which is:
--  FuncArgType : 'Tape' | 'Sym'
annotatedType :: ParserM DataType
annotatedType = SymType <$ lTok "Sym"
            <|> TapeType <$ lTok "Tape"

tapeSymbol :: ParserM TapeSymbol
tapeSymbol = noneOf "\'\""

sym :: ParserM Sym
sym = Read <$ lTok "read" <* lWhitespace <*> tapeVal
  <|> SymLit <$> between (char '\'') (lTok "\'") tapeSymbol

tape :: ParserM Tape
tape = TapeLit <$> quoted (many tapeSymbol)

anyType :: ParserM Any
anyType = S <$> sym
      <|> T <$> tape

valTyped :: (Typed a) => ParserM a -> ParserM (Val a, DataType)
valTyped p = _val <|> _var where
    _val = p >>= \x -> return (New x, typeOf x)
    _var = refVarId >>= \(name, t) -> return (Var name, t)

val :: (Typed a) => ParserM a -> ParserM (Val a)
val = (fmap fst) . valTyped

expTypeVal :: DataType -> ParserM a -> ParserM (Val a)
expTypeVal expType p = New <$> p
                   <|> Var <$> expTypeVarId expType

symVal :: ParserM (Val Sym)
symVal = expTypeVal SymType sym

tapeVal :: ParserM (Val Tape)
tapeVal = expTypeVal TapeType tape

expTypeAny :: DataType -> ParserM Any
expTypeAny SymType        = S <$> sym
expTypeAny TapeType       = T <$> tape
expTypeAny (CustomType _) = undefined

expTypeAnyVal :: DataType -> ParserM (Val Any)
expTypeAnyVal expType = expTypeVal expType (expTypeAny expType)

-- Variable which has a type, the EBNF of which is:
--  TypedVar : VarName ':' Type
typedVar :: ParserM Identifier -> ParserM (Identifier, DataType)
typedVar p = do
    name <- p
    _ <- lTok ":"
    idType <- annotatedType
    return (name, idType)

-- Parses a variable declaration, i.e. 'let x = ...'
varDecl :: ParserM Stm
varDecl = do
    _ <- lTok "let"
    name <- newVarId
    _ <- lTok "="
    (v, t) <- valTyped anyType

    putM name (PVar t)

    return (VarDecl name v)
