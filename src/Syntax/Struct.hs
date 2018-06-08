module Syntax.Struct where

import Syntax.Tree
import Syntax.ParseState

--  StructDecl    : 'struct' StructName '{' (TypedVar '\n')+ '}'
--  NewStruct     : StructName (Var ' ')+
--  MemberAccess  : VarName '.' VarName

structDecl :: ParserM Stm
structDecl = undefined
