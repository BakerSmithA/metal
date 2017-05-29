module Parser where

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

-- Parses a string enclosed in parenthesis.
parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

-- Parses a string encased in double quotes.
encasedString :: Parser String
encasedString = between (string "\"") (string "\"") (many alphaNumChar)

-- The type representing a tape symbol, i.e. a symbol contained in a cell of
-- the machine's tape.
type TapeSymbol = Char

-- Parses a tape symbol, the EBNF syntax of which is:
--  TapeSymbol  : LowerChar | UpperChar | Digit | '+' | '/'
tapeSymbol :: Parser TapeSymbol
tapeSymbol = alphaNumChar
         <|> char '+'
         <|> char '/'

-- The type represented a derived symbol, i.e. either a literal tape symbol, or
-- a symbol read from under the read/write head.
data DerivedSymbol = Literal TapeSymbol
                   | Read

-- Parses a derived symbol, the EBNF syntax of which is:
--   DerivedSymbol : 'read' | TapeSymbol
derivedSymbol :: Parser DerivedSymbol
derivedSymbol = Literal <$> tapeSymbol
            <|> Read <$ string "read"

-- The type representing a function name.
type FuncName = String

-- Parses a function name, the EBNF syntax of which is:
--  FuncName : LowerChar (LowerChar | UpperChar | Digit)*
-- A practical consideration when parsing function names is that they do not
-- conflict with reserved keywords.
funcName :: Parser FuncName
funcName = (str >>= check) where
    str        = (:) <$> letterChar <*> many alphaNumChar
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

-- Parses the basis elements of the boolean expressions, plus boolean
-- expressions wrapped in parenthesis.
bexp' :: Parser Bexp
bexp' = try (parens bexp)
    <|> TRUE  <$ string "True"
    <|> FALSE <$ string "False"
    <|> try (Eq <$> derivedSymbol <* string " == " <*> derivedSymbol)
    <|> try (Le <$> derivedSymbol <* string " <= " <*> derivedSymbol)

-- The operators that can work on boolean expressions. There is no precedence,
-- instead the expression is evaualted from left to right.
bexpOps :: [[Operator Parser Bexp]]
bexpOps = [[Prefix (Not <$ string "not ")],
           [InfixL (And <$ string " and ")]]

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
data Stm = Left
         | Right
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
         | PrintStr

-- Parses the elements of the syntactic class Stm, except for composition.
stm' :: Parser Stm
stm' = undefined

-- The operators that can work on statements.
stmOps :: [[Operator Parser Stm]]
stmOps = undefined

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
stm = undefined
