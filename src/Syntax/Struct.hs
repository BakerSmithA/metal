module Syntax.Struct where

import Syntax.Tree
import Syntax.ParseState
import Syntax.Identifier
import Syntax.Common
import Syntax.Variable
import Data.List as List (find)

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

-- Parses the name of a struct member, ensuring the variable exists in the
-- given list of members.
refStructMem :: [StructMemberVar] -> ParserM (VarName, DataType)
refStructMem mems = refVarIdWith findType where
    findType i = tryEnvDecl (fmap PVar foundType) where
        foundType = fmap snd (List.find matchName mems)
        matchName (name, _) = name == i

-- Parses the name of a member variable in a struct. Fails if the variable has
-- already been declared.
newMemberVarId :: ParserM VarName
newMemberVarId = newId snakeId

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

-- Parses access to a member of an instance of a struct. Fails if the variable
-- is not a struct, or the member is not part of the struct class.
--  MemberAccess : VarName '.' VarName
refMemberId :: ParserM (VarName, DataType)
refMemberId = do
    (structName, idType) <- refId camelId
    case idType of
        PVar (CustomType structName) -> do
            struct <- getM structName
            case struct of
                PStruct memTypes -> do
                    _ <- lTok "."
                    refStructMem memTypes
                _ -> fail "expected struct type"
        _  -> fail "expected variable to be struct type"
