module Syntax.Parser where

import Control.Monad.State.Lazy (StateT, when, modify, get, put, lift, runStateT, liftM)
import Syntax.Tree
import Syntax.ParseState as S
import Syntax.Bexp
import Syntax.Control
import Syntax.Func
import Syntax.Helper
import Syntax.Identifier
import Syntax.Variable
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.String as M

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
--  Type          : 'Tape' | 'Sym' | StructName
--
--  TapeSymbol    : LowerChar | UpperChar | Digit | ASCII-Symbol
--  TapeLiteral   : '"' TapeSymbol* '"'
--
--  DerivedValue  : 'read'
--                | VarName
--                | \' TapeSymbol \'
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
--  FuncDeclArg   : FuncDeclArg : ArgName ':' Type
--  FuncDeclArgs  : FuncDeclArg (' ' FuncDeclArg)* | ε
--  FuncDecl      : 'func' FuncName FuncDeclArgs '{' Stm '}'
--  FuncCallArg   : DerivedSymbol | TapeLiteral
--  FuncCallArgs  : FuncCallArg (',' FuncCallArg) | ε
--  Call          : FuncName FuncCallArgs
--
--  MemberDecl    : VarName ':' Type
--  StructDecl    : 'struct' StructName '{' (MemberDecl '\n')+ '}'
--  NewStruct     : StructName (Var ' ')+
--  MemberAccess  : VarName '.' VarName
--
--  Var           : DerivedValue
--                | TapeLiteral
--                | NewStruct
--  VarDecl       : 'let' VarName '=' Var
--
--  Stm           : 'left' VarName
--                | 'right' VarName
--                | 'write' VarName DerivedValue
--                | 'write' VarName String
--                | 'reject'
--                | 'accept'
--                | VarDecl
--                | StructDecl
--                | MemberAccess
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
stm' :: Parser Stm
stm' = try funcCall
   <|> MoveLeft <$ lTok "left" <* lWhitespace <*> refVar TapeType
   <|> MoveRight <$ lTok "right" <* lWhitespace <*> refVar TapeType
   <|> try (Write <$ lTok "write" <*> refVar TapeType <* lWhitespace <*> derivedSymbol SymType)
   <|> WriteStr <$ lTok "write" <*> refVar TapeType <* lWhitespace <*> quotedString
   <|> Reject <$ lTok "reject"
   <|> Accept <$ lTok "accept"
   <|> try (VarDecl <$ lTok "let" <*> newVar SymType <* lTok "=" <*> derivedSymbol SymType)
   <|> TapeDecl <$ lTok "let" <*> newVar TapeType <* lTok "=" <*> tapeLiteral
   <|> funcDecl stmComp
   <|> try (PrintStr <$ lTok "print" <*> quotedString)
   <|> try (PrintRead <$ lTok "print" <* lWhitespace <*> refVar TapeType)
   <|> DebugPrintTape <$ lTok "_printTape" <*> refVar TapeType
   <|> whileStm stmComp
   <|> ifStm stmComp

-- Composes a list of statements using Comp.
compose :: [Stm] -> Stm
compose []  = error "Compose failed: expected a statement"
compose [x] = x
compose xs  = foldr1 Comp xs

-- Parses statements separated by newlines into a composition of statements.stmComp :: Parser Stm
stmComp :: Parser Stm
stmComp = (stms <* lWhitespaceNewline) >>= (return . compose) where
    stms :: Parser [Stm]
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
stm :: Parser Stm
stm = stmComp <* lWhitespaceNewline

-- Parses an import statement, the EBNF syntax of which is given below.
--  Import : 'import ' String
importPath :: M.Parser ImportPath
importPath = tok "import" *> many (noneOf "\n\r")

-- Parses many import statements seperated by newlines.
--  Imports : ('import ' String '\n'+)*
importPaths :: M.Parser [ImportPath]
importPaths = whitespaceNewline *> paths <* whitespaceNewline where
    paths = try (many (importPath <* some newline))
        <|> (:) <$> importPath <*> pure []

-- Parses a program, the EBNF syntax of which is:
--  Program : Import* Stm
program :: Parser Stm
program = lift importPaths *> stm <* eof

-- Parses using the initial parse state, returning the new parse state.
parseRunState :: ParseState -> Parser a -> ImportPath -> FileContents -> Either (ParseError (Token String) Dec) (a, ParseState)
parseRunState initialState parser fileName fileContents = parse p fileName fileContents where
    p = runStateT parser initialState

-- Parses using the initial parse state, discarding the new parse state.
parseEvalState :: ParseState -> Parser a -> ImportPath -> FileContents -> Either (ParseError (Token String) Dec) a
parseEvalState initialState parser fileName fileContents = liftM fst (parseRunState initialState parser fileName fileContents)

-- Parses using an empty initial parse state, discarding the new parse state.
parseEmptyState :: Parser a -> ImportPath -> FileContents -> Either (ParseError (Token String) Dec) a
parseEmptyState = parseEvalState S.empty
