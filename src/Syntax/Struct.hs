module Syntax.Struct where

import Syntax.Tree
import Syntax.ParseState
import Syntax.Identifier
import Syntax.Common
import Syntax.Variable
import Syntax.Env as Env hiding (modify)
import Data.List as List (find)
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
    (name, idType) <- refId snakeId
    case idType of
        PVar (CustomType structName) -> block p where
            p = do
                addStructMemsToEnv structName
                _ <- lTok "."
                refVarId
        _ -> fail "expected variable to have struct type"

-- Assuming the struct with the given name exists, adds all the member variables
-- to the environment. Fails if the struct does not exists.
addStructMemsToEnv :: StructName -> ParserM ()
addStructMemsToEnv name = do
    idType <- getM name
    case idType of
        PStruct mems -> modify (\env -> foldr putMem env mems) where
            putMem (name, memType) = Env.put name (PVar memType)
        _ -> fail "expected struct"

-- Parses the name and type of a member variable in a struct. EBNF:
--  TypedVar: VarName ':' Type
memberVarDecl :: ParserM StructMemberVar
memberVarDecl = do
    (name, memType) <- typeAnnotated newMemberVarId
    putM name (PVar memType)
    return (name, memType)

-- Parses the member variables of a struct, failing if the variable name has
-- already been used, or the type does not exist. EBNF:
--  MemberVars: (TypedVar '\n')+
memberVarDecls :: ParserM [StructMemberVar]
memberVarDecls = memberVarDecl `sepBy` some (newline <* lWhitespace)

-- Declares a new structure. Fails if the structure already exists at this
-- scope. EBNF:
--  NewStruct : StructName (Var ' ')+ '{' MemberVars '}'
structDecl :: ParserM Stm
structDecl = StructDecl <$ lTok "struct" <*> newStruct <*> block (braces memberVarDecls)

-- Parses the construction of a new struct object. Fails if the struct does
-- not exist, or the incorrect number of arguments with the wrong types are
-- given. EBNF:
--  CreateStruct : StructName (Var ' ')+
makeObj :: ParserM Stm
makeObj = do
    (name, ms) <- refStruct
    args <- matchedTypes expTypeExpr (map memberVarType ms)
    return (MakeObj name args)

-- refMemberId = do
--     (structName, idType) <- refId camelId
--     case idType of
--         PVar (CustomType structName) -> do
--             struct <- getM structName
--             case struct of
--                 PStruct memTypes -> do
--                     _ <- lTok "."
--                     refStructMem memTypes
--                 _ -> fail "expected struct type"
--         _  -> fail "expected variable to be struct type"
