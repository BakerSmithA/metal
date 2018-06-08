module Syntax.Struct where

import Syntax.Tree
import Syntax.ParseState
import Syntax.Identifier
import Syntax.Common
import Syntax.Variable (typedVar)

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

-- Parses the name and type of a member variable in a struct. EBNF:
--  TypedVar: VarName ':' Type
memberVar :: ParserM StructMemberVar
memberVar = do
    (name, memType) <- typedVar newMemberVarId
    putM name (PVar memType)
    return (name, memType)

-- Parses the member variables of a struct, failing if the variable name has
-- already been used, or the type does not exist. EBNF:
--  MemberVars: (TypedVar '\n')+
memberVars :: ParserM [StructMemberVar]
memberVars = memberVar `sepBy` some (newline <* lWhitespace)

-- Declares a new structure. Fails if the structure already exists at this
-- scope. EBNF:
--  NewStruct : StructName (Var ' ')+ '{' MemberVars '}'
structDecl :: ParserM Stm
structDecl = StructDecl <$ lTok "struct" <*> newStruct <*> block (braces memberVars)

-- Parses the construction of a new struct object. Fails if the struct does
-- not exist, or the incorrect number of arguments with the wrong types are
-- given. EBNF:
--  CreateStruct : StructName (Var ' ')+
createStruct :: ParserM Stm
createStruct = undefined

-- Parses access to a member of an instance of a struct. Fails if the variable
-- is not a struct, or the member is not part of the struct class.
--  MemberAccess : VarName '.' VarName
memberAccess :: ParserM Stm
memberAccess = undefined
