module Syntax.Parser where

import Syntax.Tree
import Syntax.Env as E
import Syntax.Control
import Syntax.Func
import Syntax.Struct
import Syntax.Common
import Syntax.VariableExpr
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
--                | DerivedValue '==' DerivedValue
--                | DerivedValue '<=' DerivedValue
--                | DerivedValue '!=' DerivedValue
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
--  Stm           : 'left' VarName
--                | 'right' VarName
--                | 'write' VarName DerivedValue
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
--                | 'print' VarName
--                | 'print' String
--
--  Import        : 'import ' String
--  Imports       : ('import ' String '\n'+)*
--  Program       : Imports Stm

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
   <|> try (PrintStr <$ lTok "print" <*> quotedString)
   <|> try (PrintRead <$ lTok "print" <* lWhitespace <*> tapeExpr)
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
    stms :: ParserM [Stm]
    stms = try ((:) <$> (stm' <* some (newline <* lWhitespace)) <*> stms)
       <|> (:) <$> stm' <*> pure []

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
    paths = try (many (importPath <* some newline))
        <|> (:) <$> importPath <*> pure []

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
