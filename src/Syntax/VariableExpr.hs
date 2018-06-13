module Syntax.VariableExpr where

import Syntax.Common
import Syntax.Identifier
import Syntax.Env as Env hiding (modify)
import Control.Monad.State (modify)

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

-- Parses access to a member of an instance of a struct. Fails if the variable
-- is not a struct, or the member is not part of the struct class.
--  MemberAccess : VarName ('.' VarName)+
refMemberId :: ParserM (VarName, DataType)
refMemberId = do
    i <- refTopVarId
    evalMemberId i where
        -- Recursively tries to evaluate a struct member access.
        evalMemberId :: (VarName, DataType) -> ParserM (VarName, DataType)
        evalMemberId (_, CustomType structName) = block p where
            p = do
                addStructMemsToEnv structName
                -- Attempts to parse another ('.' VarName).
                lTok "." *> (try chainedAccess <|> refVarId)
            -- Attemps to parse another chained access, e.g. x.y.z
            chainedAccess = refVarId >>= evalMemberId
        evalMemberId (varName, varType) = fail msg where
            msg = "expected " ++ varName ++ " to have struct type, but got " ++ (show varType)

        -- Assuming the struct with the given name exists, adds all the member variables
        -- to the environment. Fails if the struct does not exists.
        addStructMemsToEnv :: StructName -> ParserM ()
        addStructMemsToEnv name = do
            idType <- getM name
            case idType of
                PStruct mems -> modify (\env -> foldr putMem env mems) where
                    putMem (memName, memType) = Env.put memName (PVar memType)
                _ -> fail "expected struct"

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
    case idType of
        PVar varType -> return (name, varType)
        _            -> fail "Expected variable"

-- Parses a variable any its type. The variable may exist at the top level,
-- or in a structure.
refVarId :: ParserM (VarName, DataType)
refVarId = try refMemberId
       <|> refTopVarId

-- Parses a symbol that can be written to a tape.
tapeSymbol :: ParserM TapeSymbol
tapeSymbol = noneOf "\'\""

-- Parses an expression which evaluates to a tape symbol.
symExpr :: ParserM SymExpr
symExpr = Read <$ lTok "read" <* lWhitespace <*> tapeExpr
      <|> SymLit <$> between (char '\'') (lTok "\'") tapeSymbol
      <|> SymVar <$> expVarId SymType

-- Parses an expression which evaluates to a tape reference.
tapeExpr :: ParserM TapeExpr
tapeExpr = TapeLit <$> quoted (many tapeSymbol)
       <|> TapeVar <$> expVarId TapeType

-- Parses an expression which evaluates to an object reference.
objExpr :: ParserM ObjExpr
objExpr = makeObj <|> objVar where
    makeObj = do
        (name, ms) <- refStruct
        args <- matchedTypes (maybeParens . expAnyValExpr) (map memberVarType ms)
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
expVarId :: DataType -> ParserM VarName
expVarId t = expTypeId refVarId (==t)

-- Ensures the expression which evaluates to either a tape symbol, tape, or
-- object has the correct type, e.g. object.
expAnyValExpr :: DataType -> ParserM AnyValExpr
expAnyValExpr = expDataType anyValExpr
