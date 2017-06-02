module Syntax.Parser where

import Syntax.Tree
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
--  VarName       : LowerChar (LowerChar | UpperChar | Digit)*
--  FuncName      : LowerChar (LowerChar | UpperChar | Digit)*
--  TapeSymbol    : LowerChar | UpperChar | Digit | ASCII-Symbol
--  DerivedSymbol : 'read'
--                | VarName
--                | \' TapeSymbol \'
--  Bexp          : 'True'
--                | 'False'
--                | 'not' Bexp
--                | Bexp 'and' Bexp
--                | Bexp 'or' Bexp
--                | DerivedSymbol '==' DerivedSymbol
--                | DerivedSymbol '<=' DerivedSymbol
--  Else          : 'else' { Stm } | ε
--  ElseIf        : 'else if' { Stm } ElseIf | Else
--  If            : 'if' { Stm } ElseIf
--  Stm           : 'left'
--                | 'right'
--                | 'write' DerivedSymbol
--                | 'reject'
--                | 'accept'
--                | 'let' VarName '=' DerivedSymbol
--                | 'func' FuncName '{' Stm '}'
--                | FuncName
--                | Stm '\n' Stm
--                | 'print'
--                | 'print' String
--                | 'while' Bexp '{' Stm '}'
--                | If

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["space", "read", "True", "False", "not", "and", "or",
                    "left", "right", "write", "reject", "accept", "let", "if",
                    "else", "while", "call", "print"]

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
whitespace = whitespaceConsumer separatorChar

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
braces p = between openBrace closeBrace p' where
    p'         = p <* whitespaceNewline
    openBrace  = tok "{" <* optional newline
    closeBrace = tok "}"

-- Parses a string encased in double quotes.
encasedString :: Parser String
encasedString = between (tok "\"") (tok "\"") (many (noneOf "\""))

-- Parses a tape symbol, the EBNF syntax of which is:
--  TapeSymbol  : LowerChar | UpperChar | Digit | ASCII-Symbol
tapeSymbol :: Parser TapeSymbol
tapeSymbol = asciiChar <* whitespace

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

-- Parses a function name, the EBNF syntax of which is:
--  FuncName : LowerChar (LowerChar | UpperChar | Digit)*
funcName :: Parser FuncName
funcName = identifier

-- Parses a derived symbol, the EBNF syntax of which is:
--  DerivedSymbol : 'read'
--                | VarName
--                | \' TapeSymbol \'
derivedSymbol :: Parser DerivedSymbol
derivedSymbol = Read <$ tok "read"
            <|> Var <$> varName
            <|> Literal <$ tok "'" <*> tapeSymbol <* tok "'"

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

-- Parses if-else statement, the EBNF syntax of which is:
--  If     : 'if' { Stm } ElseIf
--  ElseIf : 'else if' { Stm } ElseIf | Else
--  Else   : 'else' { Stm } | ε
ifStm :: Parser Stm
ifStm = If <$ tok "if" <*> bexp <*> braces stm <*> many elseIfClause <*> elseClause where
    elseIfClause = do
        b <- tok "else if" *> bexp
        stm <- braces stm
        return (b, stm)
    elseClause = optional (tok "else" *> braces stm)

-- Parses the elements of the syntactic class Stm, except for composition.
stm' :: Parser Stm
stm' = MoveLeft <$ tok "left"
   <|> MoveRight <$ tok "right"
   <|> Write <$ tok "write" <*> derivedSymbol
   <|> Reject <$ tok "reject"
   <|> Accept <$ tok "accept"
   <|> VarDecl <$ tok "let" <*> varName <* tok "=" <*> derivedSymbol
   <|> FuncDecl <$ tok "func" <*> funcName <*> braces stm
   <|> try (Call <$> funcName)
   <|> try (PrintStr <$ tok "print" <*> encasedString)
   <|> try (PrintRead <$ tok "print")
   <|> While <$ tok "while" <*> bexp <*> braces stm
   <|> ifStm

-- The operators that can work on statements - this is used to parse composition.
stmOps :: [[Operator Parser Stm]]
stmOps = [[InfixL (Comp <$ some newline <* whitespace)]]

-- Parses a statement, the EBNF syntax of statements being:
--  Stm : 'left'
--      | 'right'
--      | 'write' DerivedSymbol
--      | 'reject'
--      | 'accept'
--      | 'let' VarName '=' DerivedSymbol
--      | 'func' FuncName '{' Stm '}'
--      | FuncName
--      | Stm '\n' Stm
--      | 'print'
--      | 'print' String
--      | 'while' Bexp '{' Stm '}'
--      | If
stm :: Parser Stm
stm = whitespaceNewline *> (try (makeExprParser stm' stmOps) <|> stm')
