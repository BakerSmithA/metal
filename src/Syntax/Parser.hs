module Syntax.Parser where

import Syntax.Tree
import Syntax.Env as E
import Syntax.Control
import Syntax.Func
import Syntax.Common
import Syntax.VariableExpr
import Syntax.Identifier
import qualified Text.Megaparsec.String as M
import Control.Monad.State.Lazy (runStateT, lift, liftM)

-- Abstract Grammar
--
--  LowerChar     : 'a' | 'b' | ... | 'z'
--  UpperChar     : 'A' | 'B' | ... | 'Z'
--  Digit         : '0' | '1' | ... | '9'
--  String        : " (LowerChar | UpperChar | Digit)* "
--
--  Identifier    : LowerChar (LowerChar | UpperChar | Digit)*
--  VarName       : Identifier
--  FuncName      : Identifier
--  ArgName       : Identifier
--  StructName    : Identifier
--  TypeAnnotation: 'Tape' | 'Sym' | StructName
--  TypedVar      : VarName ':' Type
--
--  TapeSymbol    : LowerChar | UpperChar | Digit | ASCII-Symbol
--  TapeLiteral   : '"' TapeSymbol* '"'
--
--  VarExpr       : VarName
--                | MemberAccess
--  SymExpr       : 'read' TapeExpr
--                | \' TapeSymbol \'
--  TapeExpr      : \" many TapeSymbol \"
--  ObjExpr       : NewObj
--  AnyTypeExpr   : SymExpr
--                | TapeExpr
--                | ObjExpr
--                | VarExpr
--
--  VarDecl       : 'let' VarName '=' AnyTypeExpr
--
--  Bexp          : 'True'
--                | 'False'
--                | 'not' Bexp
--                | Bexp 'and' Bexp
--                | Bexp 'or' Bexp
--                | SymExpr '==' SymExpr
--                | SymExpr '<=' SymExpr
--                | SymExpr '!=' SymExpr
--  Else          : 'else' { Stm } | ε
--  ElseIf        : 'else if' { Stm } ElseIf | Else
--  If            : 'if' { Stm } ElseIf
--
--  FuncDeclArgs  : ArgName (' ' TypedVar)* | ε
--  FuncDecl      : 'func' FuncName FuncDeclArgs '{' Stm '}'
--  FuncCallArg   : AnyTypeExpr
--  FuncCallArgs  : FuncCallArg (',' FuncCallArg) | ε
--  Call          : FuncName FuncCallArgs
--
--  MemberVars    : (TypedVar '\n')+
--  StructDecl    : 'struct' StructName '{' MemberVars '}'
--  NewObj        : StructName (AnyTypeExpr ' ')+
--  MemberAccess  : VarName '.' VarName
--
--  Stm           : 'left' TapeExpr
--                | 'right' TapeExpr
--                | 'write' TapeExpr SymExpr
--                | 'reject'
--                | 'accept'
--                | VarDecl
--                | StructDecl
--                | NewObj
--                | If
--                | 'while' Bexp '{' Stm '}'
--                | FuncDecl
--                | Call
--                | Stm '\n' Stm
--                | 'print' SymExpr
--
--  Import        : 'import ' String
--  Imports       : ('import ' String '\n'+)*
--  Program       : Imports Stm

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
memberVarDecls structName = (memberVarDecl structName) `sepEndBy` some (newline <* lWhitespace)

-- Declares a new structure. Fails if the structure already exists at this
-- scope. EBNF:
--  NewStruct : StructName (Var ' ')+ '{' MemberVars '}'
structDecl :: ParserM Stm
structDecl = do
    name <- lTok "struct" *> newStructId
    mems <- block (braces (memberVarDecls name))
    return (StructDecl name mems)

-- Parses a variable (e.g. tape, symbol, object) declaration, EBNF:
--  'let' VarName '=' AnyTypeExpr
varDecl :: ParserM Stm
varDecl = do
    _ <- lTok "let"
    name <- newVarId
    _ <- lTok "="
    v <- anyValExpr

    putM name (PVar (typeOf v))

    return (VarDecl name v)

-- Parses the elements of the syntactic class Stm, except for composition.
stm' :: ParserM Stm
stm' = try funcCall
   <|> MoveLeft <$ lTok "left" <* lWhitespace <*> tapeExpr
   <|> MoveRight <$ lTok "right" <* lWhitespace <*> tapeExpr
   <|> try (Write <$ lTok "write" <*> tapeExpr <* lWhitespace <*> symExpr)
   <|> Reject <$ lTok "reject"
   <|> Accept <$ lTok "accept"
   <|> varDecl
   <|> funcDecl stmComp
   <|> try (Print <$ lTok "print" <* lWhitespace <*> symExpr)
   <|> DebugPrintTape <$ lTok "_print" <*> tapeExpr
   <|> whileStm stmComp
   <|> ifStm stmComp
   <|> structDecl

-- Composes a list of statements using Comp.
compose :: [Stm] -> Stm
compose []  = error "Compose failed: expected a statement"
compose [x] = x
compose xs  = foldr1 Comp xs

-- Parses statements separated by newlines into a composition of statements.stmComp :: ParserM Stm
stmComp :: ParserM Stm
stmComp = (stms <* lWhitespaceNewline) >>= (return . compose) where
    stms = stm' `sepEndBy` some (newline <* lWhitespace)

-- Parses a statement, the EBNF syntax of which is given below. The parser will
-- fail if not all input is consumed.
--  Stm : 'left'
--      | 'right'
--      | 'write' DerivedValue
--      | 'reject'
--      | 'accept'
--      | 'let' VarName '=' DerivedValue
--      | If
--      | 'while' Bexp '{' Stm '}'
--      | FuncDecl
--      | Call
--      | Stm '\n' Stm
--      | 'print'
--      | 'print' String
--      | Import
stm :: ParserM Stm
stm = stmComp <* lWhitespaceNewline

-- Parses an import statement, the EBNF syntax of which is given below.
--  Import : 'import ' String
importPath :: Parser ImportPath
importPath = tok "import" *> many (noneOf "\n\r")

-- Parses many import statements seperated by newlines.
--  Imports : ('import ' String '\n'+)*
importPaths :: Parser [ImportPath]
importPaths = whitespaceNewline *> paths <* whitespaceNewline where
    paths = importPath `sepEndBy` some newline

-- Parses a program, the EBNF syntax of which is:
--  Program : Import* Stm
program :: ParserM Stm
program = lift importPaths *> stm <* eof

-- Parses using the initial parse state, returning the new parse state.
parseRunState :: ParseState -> ParserM a -> ImportPath -> FileContents -> Either (ParseError (Token String) Dec) (a, ParseState)
parseRunState initialState parser fileName fileContents = parse p fileName fileContents where
    p = runStateT parser initialState

-- Parses using the initial parse state, discarding the new parse state.
parseEvalState :: ParseState -> ParserM a -> ImportPath -> FileContents -> Either (ParseError (Token String) Dec) a
parseEvalState initialState parser fileName fileContents = liftM fst (parseRunState initialState parser fileName fileContents)

-- Parses using an empty initial parse state, discarding the new parse state.
parseEmptyState :: ParserM a -> ImportPath -> FileContents -> Either (ParseError (Token String) Dec) a
parseEmptyState = parseEvalState E.empty
