module Syntax.VariableExpr where

import Syntax.Common
import Syntax.Identifier
import Syntax.Env as Env hiding (modify)
import Syntax.ParseState as ParseState
import Control.Monad.State as State (put, get)

import Debug.Trace

-- Parses the name of a struct, making sure the struct does not already exist.
newStructId :: ParserM StructName
newStructId = newId camelId

-- Parses the name of a struct, ensuring it already exists.
refStruct :: ParserM (StructName, [StructMemberVar])
refStruct = do
    (name, idType) <- refId camelId
    case idType of
        PStruct ms -> return (name, ms)
        _          -> fail "Expected struct"

-- Parses the name of a member variable in a struct. Fails if the variable has
-- already been declared.
newMemberVarId :: ParserM VarName
newMemberVarId = newId snakeId

-- Recursively tries to evaluate a struct member access.
refChainedMemberId :: ParseState -> VarPath -> DataType -> ParserM (VarPath, DataType)
refChainedMemberId baseState path (CustomType structName) = do
    mems <- getStruct structName baseState
    State.put (ParseState.fromVars mems)
    (name, t) <- lTok "." *> refTopVarId
    chaninedAccess path name t <|> finalAccess path name t where
        chaninedAccess path name t = refChainedMemberId baseState (path ++ [name]) t
        finalAccess    path name t = return (path ++ [name], t)
refChainedMemberId _ path t = fail "Expected struct"

-- Parses access to a member of an instance of a struct. Fails if the variable
-- is not a struct, or the member is not part of the struct class.
--  MemberAccess : VarName ('.' VarName)+
refMemberId :: ParserM (VarPath, DataType)
refMemberId = do
    (name, t) <- refTopVarId
    baseState <- State.get
    block $ refChainedMemberId baseState [name] t

-- Attempts to parse an identifier used to declare a new variable.
-- Fails if the variable already exists. If the variable does not exist
-- it is added to the environment.
newVarId :: ParserM VarName
newVarId = newId snakeId

-- Parses a variable and its type. Fails if the variable does not already
-- exist in the top level of the environment, i.e. not in any structures.
refTopVarId :: ParserM (VarName, DataType)
refTopVarId = do
    (name, idType) <- refId snakeId
    trace ("Var " ++  name) $ case idType of
        PVar varType -> return (name, varType)
        _            -> fail "Expected variable"

-- Parses a variable any its type. The variable may exist at the top level,
-- or in a structure.
refVarId :: ParserM (VarPath, DataType)
refVarId = try refMemberId
       <|> fmap (\(n, t) -> ([n], t)) refTopVarId

-- Parses a symbol that can be written to a tape.
tapeSymbol :: ParserM TapeSymbol
tapeSymbol = noneOf "\'\""

-- Parses an expression which evaluates to a tape symbol.
symExpr :: ParserM SymExpr
symExpr = Read <$ lTok "read" <* lWhitespace <*> tapeExpr
      <|> SymLit <$> between (char '\'') (lTok "\'") tapeSymbol
      <|> SymVar <$> expVarId SymType
      <|> parens symExpr

-- Parses an expression which evaluates to a tape reference.
tapeExpr :: ParserM TapeExpr
tapeExpr = TapeLit <$> quoted (many tapeSymbol)
       <|> TapeVar <$> expVarId TapeType
       <|> parens tapeExpr

-- Parses an expression which evaluates to an object reference.
objExpr :: ParserM ObjExpr
objExpr = makeObj <|> objVar <|> parens objExpr where
    makeObj = do
        (name, ms) <- refStruct
        args <- matchedTypes expAnyValExpr (map memberVarType ms)
        return (NewObj name args)
    objVar = do
        (name, (CustomType structName)) <- expType refVarId (isCustomType . snd)
        return (ObjVar structName name)

-- Parses an expression which evaluates to either a tape symbol, tape reference,
-- or object reference.
anyValExpr :: ParserM AnyValExpr
anyValExpr = try (S <$> symExpr)
         <|> try (T <$> tapeExpr)
         <|> try (C <$> objExpr)

-- Ensures the referenced variable has the given type, otherwise fails.
expVarId :: DataType -> ParserM VarPath
expVarId t = expTypeId refVarId (==t)

-- Ensures the expression which evaluates to either a tape symbol, tape, or
-- object has the correct type, e.g. object.
expAnyValExpr :: DataType -> ParserM AnyValExpr
expAnyValExpr = expDataType anyValExpr
