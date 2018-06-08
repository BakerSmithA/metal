module Syntax.Struct where

import Syntax.Tree
import Syntax.ParseState
import Syntax.Identifier
import Syntax.Common

--  MemberAccess  : VarName '.' VarName

-- Parses the name of a struct, making sure the struct does not already exist.
newStruct :: ParserM StructName
newStruct = newId camelId

-- Parses the name of a struct, ensuring it already exists.
refStruct :: ParserM (StructName, [StructMemberVar])
refStruct = do
    (name, members) <- refId camelId
    case members of
        PStruct ms -> return (name, ms)
        _          -> fail "Expected struct"

-- Parses the member variables of a struct, failing if the variable name has
-- already been used, or the type does not exist. EBNF:
--  MemberVars: (TypedVar '\n')+
structMemberVars :: ParserM [StructMemberVar]
structMemberVars = undefined

-- Declares a new structure. Fails if the structure already exists at this
-- scope. EBNF:
--  NewStruct : StructName (Var ' ')+ '{' MemberVars '}'
structDecl :: ParserM Stm
structDecl = StructDecl <$ lTok "struct" <*> newStruct <*> structMemberVars
