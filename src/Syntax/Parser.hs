module Syntax.Parser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L

-- Abstract Grammar
--
--  LowerChar     : 'a' | 'b' | ... | 'z'
--  UpperChar     : 'A' | 'B' | ... | 'Z'
--  Digit         : '0' | '1' | ... | '9'
--  String        : " (LowerChar | UpperChar | Digit)* "
--  TapeSymbol    : LowerChar | UpperChar | Digit | '+' | '/'
--  DerivedSymbol : 'read' | TapeSymbol
--  FuncName      : LowerChar (LowerChar | UpperChar | Digit)*
--  Bexp          : 'True'
--                | 'False'
--                | 'not' Bexp
--                | Bexp 'and' Bexp
--                | Bexp 'or' Bexp
--                | DerivedSymbol '==' DerivedSymbol
--                | DerivedSymbol '<=' DerivedSymbol
--  Stm           : 'left'
--                | 'right'
--                | 'write' TapeSymbol
--                | 'reject'
--                | 'accept'
--                | 'if' Bexp '{' Stm '}'
--                | 'if' Bexp '{' Stm '} else {' Stm '}'
--                | 'while' Bexp '{' Stm '}'
--                | 'func' FuncName '{' Stm '}'
--                | 'call' FuncName
--                | Stm '\n' Stm
--                | 'print'
--                | 'print' String

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["read", "True", "False", "not", "and", "or", "left", "right",
                    "write", "reject", "accept", "if", "else", "while", "call",
                    "print"]

-- Consumes whole line and in-line comments. The syntax for both comment types
-- are the same as C, with '//' indicating a whole line comment and '/* ... */'
-- indicating an in-line comment.
whitespace :: Parser ()
whitespace = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace.
tok :: String -> Parser String
tok s = string s <* whitespace

-- Parses a string enclosed in parenthesis.
parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

-- Parses a string enclosed in curly braces.
braces :: Parser a -> Parser a
braces = between (tok "{") (tok "}")

-- Parses a string encased in double quotes.
encasedString :: Parser String
encasedString = between (tok "\"") (tok "\"") (many (noneOf "\""))

-- The type representing a tape symbol, i.e. a symbol contained in a cell of
-- the machine's tape.
type TapeSymbol = Char

-- Parses a tape symbol, the EBNF syntax of which is:
--  TapeSymbol  : LowerChar | UpperChar | Digit | '+' | '/'
tapeSymbol :: Parser TapeSymbol
tapeSymbol = tapeSymbol' <* whitespace where
    tapeSymbol' = alphaNumChar
             <|> char '+'
             <|> char '/'

-- The type represented a derived symbol, i.e. either a literal tape symbol, or
-- a symbol read from under the read/write head.
data DerivedSymbol = Literal TapeSymbol
                   | Read
                   deriving (Eq, Show)

-- Parses a derived symbol, the EBNF syntax of which is:
--   DerivedSymbol : 'read' | TapeSymbol
derivedSymbol :: Parser DerivedSymbol
derivedSymbol = Read <$ tok "read"
            <|> Literal <$> tapeSymbol

-- The type representing a function name.
type FuncName = String

-- Parses a function name, the EBNF syntax of which is:
--  FuncName : LowerChar (LowerChar | UpperChar | Digit)*
-- A practical consideration when parsing function names is that they do not
-- conflict with reserved keywords.
funcName :: Parser FuncName
funcName = (str >>= check) <* whitespace where
    str        = (:) <$> lowerChar <*> many alphaNumChar
    check word = if word `elem` reservedKeywords
                    then fail $ "keyword " ++ show word ++ " cannot be an identifier"
                    else return word

-- The type representing the syntax tree for boolean expressions.
data Bexp = TRUE
          | FALSE
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp
          | Eq DerivedSymbol DerivedSymbol
          | Le DerivedSymbol DerivedSymbol
          deriving (Eq, Show)

-- Parses the basis elements of the boolean expressions, plus boolean
-- expressions wrapped in parenthesis.
bexp' :: Parser Bexp
bexp' = try (parens bexp)
    <|> TRUE  <$ tok "True"
    <|> FALSE <$ tok "False"
    <|> try (Eq <$> derivedSymbol <* tok "==" <*> derivedSymbol)
    <|> try (Le <$> derivedSymbol <* tok "<=" <*> derivedSymbol)

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
--       | DerivedSymbol '==' DerivedSymbol
--       | DerivedSymbol '<=' DerivedSymbol
bexp :: Parser Bexp
bexp = makeExprParser bexp' bexpOps

-- The type that represents the syntax tree for statements.
data Stm = MoveLeft
         | MoveRight
         | Write TapeSymbol
         | Reject
         | Accept
         | If Bexp Stm
         | IfElse Bexp Stm Stm
         | While Bexp Stm
         | Func FuncName Stm
         | Call FuncName
         | Comp Stm Stm
         | PrintRead
         | PrintStr String
         deriving (Eq, Show)

-- Parses the elements of the syntactic class Stm, except for composition.
stm' :: Parser Stm
stm' = MoveLeft <$ tok "left"
   <|> MoveRight <$ tok "right"
   <|> Write <$ tok "write" <*> tapeSymbol
   <|> Reject <$ tok "reject"
   <|> Accept <$ tok "accept"
   <|> try (IfElse <$ tok "if" <*> bexp <*> braces stm <* tok "else" <*> braces stm)
   <|> If <$ tok "if" <*> bexp <*> braces stm
   <|> While <$ tok "while" <*> bexp <*> braces stm
   <|> Func <$ tok "func" <*> funcName <*> braces stm
   <|> Call <$ tok "call" <*> funcName
   <|> try (PrintStr <$ tok "print" <*> encasedString)
   <|> PrintRead <$ tok "print"

-- The operators that can work on statements.
stmOps :: [[Operator Parser Stm]]
stmOps = [[InfixL (Comp <$ tok ";")]]

-- Parses a statement, the EBNF syntax of statements being:
--  Stm : 'left'
--      | 'right'
--      | 'write' TapeSymbol
--      | 'reject'
--      | 'accept'
--      | 'if' Bexp '{' Stm '}'
--      | 'if' Bexp '{' Stm '} else {' Stm '}'
--      | 'while' Bexp '{' Stm '}'
--      | 'func' FuncName '{' Stm '}'
--      | 'call' FuncName
--      | 'print'
--      | 'print' String
stm :: Parser Stm
stm = makeExprParser stm' stmOps
