module Syntax.Struct where

import Syntax.Tree
import Syntax.ParseState
import Syntax.Identifier
import Syntax.Common
import Syntax.Env as Env hiding (modify)
import Control.Monad.State (modify)

-- Parses the name of a struct, making sure the struct does not already exist.
newStruct :: ParserM StructName
newStruct = newId camelId

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
    i <- refVarId
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

-- Parses the name and type of a member variable in a struct. Given the struct
-- name to allow for recursive structures. EBNF:
--  TypedVar: VarName ':' Type
memberVarDecl :: StructName -> ParserM StructMemberVar
memberVarDecl structName = do
    (name, memType) <- recTypeAnnotated structName newMemberVarId
    putM name (PVar memType)
    return (name, memType)

-- Parses the member variables of a struct, failing if the variable name has
-- already been used, or the type does not exist. Given the struct name to
-- allow for recursive structures. EBNF:
--  MemberVars: (TypedVar '\n')+
memberVarDecls :: StructName -> ParserM [StructMemberVar]
memberVarDecls structName = (memberVarDecl structName) `sepBy` some (newline <* lWhitespace)

-- Declares a new structure. Fails if the structure already exists at this
-- scope. EBNF:
--  NewStruct : StructName (Var ' ')+ '{' MemberVars '}'
structDecl :: ParserM Stm
structDecl = do
    name <- lTok "struct" *> newStruct
    mems <- block (braces (memberVarDecls name))
    return (StructDecl name mems)
