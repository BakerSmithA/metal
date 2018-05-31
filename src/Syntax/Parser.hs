module Syntax.Parser where

import Control.Monad (void)
import Syntax.Tree
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

-- Abstract Grammar
--
--  LowerChar     : 'a' | 'b' | ... | 'z'
--  UpperChar     : 'A' | 'B' | ... | 'Z'
--  Digit         : '0' | '1' | ... | '9'
--  String        : " (LowerChar | UpperChar | Digit)* "
--  VarName       : LowerChar (LowerChar | UpperChar | Digit)*
--  VarName       : LowerChar (LowerChar | UpperChar | Digit)*
--  FuncName      : LowerChar (LowerChar | UpperChar | Digit)*
--  ArgName       : LowerChar (LowerChar | UpperChar | Digit)*
--  TapeSymbol    : LowerChar | UpperChar | Digit | ASCII-Symbol
--  TapeLiteral   : '"' TapeSymbol* '"'
--  DerivedValue  : 'read'
--                | VarName
--                | \' TapeSymbol \'
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
--  FuncArgType   : 'Tape' | 'Sym'
--  FuncDeclArg   : FuncDeclArg : ArgName ':' FuncArgType
--  FuncDeclArgs  : FuncDeclArg (' ' FuncDeclArg)* | ε
--  FuncDecl      : 'func' FuncName FuncDeclArgs '{' Stm '}'
--  FuncCallArg   : DerivedSymbol | TapeLiteral
--  FuncCallArgs  : FuncCallArg (',' FuncCallArg) | ε
--  Call          : FuncName FuncCallArgs
--  Stm           : 'left' VarName
--                | 'right' VarName
--                | 'write' VarName DerivedValue
--                | 'write' VarName String
--                | 'reject'
--                | 'accept'
--                | 'let' VarName '=' DerivedValue
--                | 'let' VarName '=' TapeLiteral
--                | If
--                | 'while' Bexp '{' Stm '}'
--                | FuncDecl
--                | Call
--                | Stm '\n' Stm
--                | 'print' VarName
--                | 'print' String
--  Import        : 'import ' String
--  Program       : Import* Stm

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["read", "True", "False", "not", "and", "or", "left",
                    "right", "write", "reject", "accept", "let", "if", "else",
                    "while", "print", "func", "import", "_printTape"]

-- Produces a whitespace consumer using `sc` as the space consumer. Consumes
-- whole line and in-line comments. The syntax for both comment types are the
-- same as C, with '//' indicating a whole line comment and '/* ... */'
-- indicating an in-line comment.
whitespaceConsumer :: Parser Char -> Parser ()
whitespaceConsumer sc = L.space (void sc) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//" <* void (many newline)
        blockCmnt = L.skipBlockComment "/*" "*/" <* void (many newline)

-- Comsumes whitespace and comments, but not newlines.
whitespace :: Parser ()
whitespace = whitespaceConsumer (oneOf "\t ")

-- Consumes whitespace, comments, and newlines.
whitespaceNewline :: Parser ()
whitespaceNewline = whitespaceConsumer spaceChar

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
tok :: String -> Parser String
tok s = string s <* whitespace

-- Parses a string enclosed in parenthesis.
parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

-- Parses a string enclosed in curly braces.
braces :: Parser a -> Parser a
braces = between (tok "{" <* whitespaceNewline) (tok "}")

quoted :: Parser a -> Parser a
quoted = between (tok "\"") (tok "\"")

-- Parses a string encased in double quotes.
quotedString :: Parser String
quotedString = quoted (many (noneOf "\""))

-- Parses a tape symbol, the EBNF syntax of which is:
--  TapeSymbol  : LowerChar | UpperChar | Digit | ASCII-Symbol
tapeSymbol :: Parser TapeSymbol
tapeSymbol = noneOf "\'\""

-- Parses an identifier, i.e. variable or function name, the EBNF syntax for
-- both being:
--  LowerChar (LowerChar | UpperChar | Digit)*
-- A practical consideration when parsing identifiers is that they do not
-- conflict with reserved keywords.
identifier :: Parser String
identifier = (str >>= check) <* whitespace where
    str        = (:) <$> lowerChar <*> many alphaNumChar
    check word = if word `elem` reservedKeywords
                    then fail $ "keyword " ++ show word ++ " cannot be an identifier"
                    else return word

-- Parses a variable name, the EBNF syntax of which is:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
varName :: Parser VarName
varName = identifier

-- Parses a tape name, the EBNF syntax of which is:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
tapeName :: Parser VarName
tapeName = identifier

-- Parses a function name, the EBNF syntax of which is:
--  FuncName : LowerChar (LowerChar | UpperChar | Digit)*
funcName :: Parser FuncName
funcName = identifier

-- Parses a function argument, the EBNF syntax of which is:
--  ArgName : LowerChar (LowerChar | UpperChar | Digit)*
argName :: Parser ArgName
argName = identifier

-- Parses a derived symbol, the EBNF syntax of which is:
--  DerivedValue : 'read'
--                | VarName
--                | \' TapeSymbol \'
derivedSymbol :: Parser DerivedValue
derivedSymbol = Read <$ tok "read" <* whitespace <*> tapeName
            <|> Var <$> varName
            <|> Literal <$> between (char '\'') (tok "\'") tapeSymbol
            <|> parens derivedSymbol

-- Parses the basis elements of the boolean expressions, plus boolean
-- expressions wrapped in parenthesis.
bexp' :: Parser Bexp
bexp' = try (parens bexp)
    <|> TRUE  <$ tok "True"
    <|> FALSE <$ tok "False"
    <|> try (Eq <$> derivedSymbol <* tok "==" <*> derivedSymbol)
    <|> try (Le <$> derivedSymbol <* tok "<=" <*> derivedSymbol)
    <|> try (Ne <$> derivedSymbol <* tok "!=" <*> derivedSymbol)

-- The operators that can work on boolean expressions. There is no precedence,
-- instead the expression is evaualted from left to right.
bexpOps :: [[Operator Parser Bexp]]
bexpOps = [[Prefix (Not <$ tok "not")],
           [InfixL (And <$ tok "and"), InfixL (Or <$ tok "or")]]

-- Parses a boolean expression, allowing for parenthesis to specify the
-- intended parse. The EBNF syntax of boolean expressions is:
--  Bexp : 'True'
--       | 'False'
--       | 'not' Bexp
--       | Bexp 'and' Bexp
--       | Bexp 'or' Bexp
--       | DerivedValue '==' DerivedValue
--       | DerivedValue '<=' DerivedValue
bexp :: Parser Bexp
bexp = makeExprParser bexp' bexpOps

-- Parses if-else statement, the EBNF syntax of which is:
--  If     : 'if' { Stm } ElseIf
--  ElseIf : 'else if' { Stm } ElseIf | Else
--  Else   : 'else' { Stm } | ε
ifStm :: Parser Stm
ifStm = If <$ tok "if" <*> bexp <*> braces stmComp <*> many elseIfClause <*> elseClause where
    elseIfClause = do
        bool <- tok "else if" *> bexp
        statement <- braces stmComp
        return (bool, statement)
    elseClause = optional (tok "else" *> braces stmComp)

-- Type of data passed to a function, the EBNF of which is:
--  FuncArgType   : 'Tape' | 'Sym'
dataType :: Parser DataType
dataType = SymType <$ tok "Sym"
       <|> TapeType <$ tok "Tape"

-- Argument to a function, the EBNF is:
--  FuncDeclArg : ArgName ':' FuncArgType
funcDeclArg :: Parser FuncDeclArg
funcDeclArg = FuncDeclArg <$> argName <* tok ":" <*> dataType

-- Parses argument names of a function declaration, the EBNF syntax of which is:
--  FuncDeclArgs : FuncDeclArg (' ' FuncDeclArg)* | ε
funcDeclArgs :: Parser FuncDeclArgs
funcDeclArgs = funcDeclArg `sepBy` whitespace

-- Parses a function declaration, the EBNF syntax of which is:
--  FuncDecl : 'func' FuncName FuncDeclArgs '{' Stm '}'
funcDecl :: Parser Stm
funcDecl = FuncDecl <$ tok "func" <*> funcName <*> funcDeclArgs <*> braces stmComp

-- Parsers the contents of a tape literal, e.g. "abcd"
tapeLiteral :: Parser String
tapeLiteral = quoted (many tapeSymbol)

-- Parses an argument to a function call, the EBNF syntax of which is:
--  FuncCallArg : DerivedValue | TapeLiteral
funcCallArg :: Parser FuncCallArg
funcCallArg = Derived <$> derivedSymbol

-- Parses the arguments supplied to a function call, the EBNF syntax of which is:
--  FuncCallArgs : FuncCallArg (',' FuncCallArg) | ε
funcCallArgs :: Parser FuncCallArgs
funcCallArgs = funcCallArg `sepBy` whitespace

-- Parses a function call, the EBNF syntax of which is:
--  Call : FuncName FuncCallArgs
funcCall :: Parser Stm
funcCall = Call <$> funcName <*> funcCallArgs

-- Parses the elements of the syntactic class Stm, except for composition.
stm' :: Parser Stm
stm' = try funcCall
   <|> MoveLeft <$ tok "left" <* whitespace <*> tapeName
   <|> MoveRight <$ tok "right" <* whitespace <*> tapeName
   <|> try (Write <$ tok "write" <*> tapeName <* whitespace <*> derivedSymbol)
   <|> WriteStr <$ tok "write" <*> tapeName <* whitespace <*> quotedString
   <|> Reject <$ tok "reject"
   <|> Accept <$ tok "accept"
   <|> try (VarDecl <$ tok "let" <*> varName <* tok "=" <*> derivedSymbol)
   <|> TapeDecl <$ tok "let" <*> tapeName <* tok "=" <*> tapeLiteral
   <|> funcDecl
   <|> try (PrintStr <$ tok "print" <*> quotedString)
   <|> try (PrintRead <$ tok "print" <* whitespace <*> tapeName)
   <|> DebugPrintTape <$ tok "_printTape" <*> varName
   <|> While <$ tok "while" <*> bexp <*> braces stmComp
   <|> ifStm

-- Composes a list of statements using Comp.
compose :: [Stm] -> Stm
compose []  = error "Compose failed: expected a statement"
compose [x] = x
compose xs  = foldr1 Comp xs

-- Parses statements separated by newlines into a composition of statements.
stmComp :: Parser Stm
stmComp = (stms <* whitespaceNewline) >>= (return . compose) where
    stms :: Parser [Stm]
    stms = try ((:) <$> (stm' <* some (newline <* whitespace)) <*> stms)
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
stm = stmComp <* whitespaceNewline

-- Parses an import statement, the EBNF syntax of which is given below. An
-- example of a valid file path is: Directory.SubDirectory.File
--  Import : 'import ' String
importStm :: Parser ImportPath
importStm = tok "import" *> many (noneOf "\n\r")

-- Parses a program, the EBNF syntax of which is:
--  Program : Import* Stm
program :: Parser Program
program = Program <$ whitespaceNewline <*> imports <*> stm <* eof where
    imports = many (importStm <* newline) <* whitespaceNewline
