module Syntax.Struct where

import Syntax.Helper
import Syntax.Variable
import Syntax.Identifier

--  StructDecl    : 'struct' StructName '{' (TypedVar '\n')+ '}'
--  NewStruct     : StructName (Var ' ')+
--  MemberAccess  : VarName '.' VarName
