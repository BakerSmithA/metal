module Syntax.Struct where

import Syntax.Helper
import Syntax.Variable
import Syntax.Identifier

--  StructDecl    : 'struct' StructName '{' (TypedVar '\n')+ '}'
--  NewStruct     : StructName (Var ' ')+
--  MemberAccess  : VarName '.' VarName

-- Parses the name of a member of a struct, the EBNF of which is the same as a
-- TypedVar. Fails if a member with that name already exists.
memberDecl :: Parser VarName
memberDecl = undefined
