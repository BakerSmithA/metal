module Syntax.Parser where

import Control.Monad (void)
import Data.Maybe
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
--  FuncName      : LowerChar (LowerChar | UpperChar | Digit)*
--  ArgName       : LowerChar (LowerChar | UpperChar | Digit)*
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
--                | DerivedSymbol '!=' DerivedSymbol
--  Else          : 'else' { Stm } | ε
--  ElseIf        : 'else if' { Stm } ElseIf | Else
--  If            : 'if' { Stm } ElseIf
--  FuncDeclArgs  : ArgName (',' ArgName)* | ε
--  FuncDecl      : 'func' FuncName FuncDeclArgs '{' Stm '}'
--  FuncCallArgs  : DerivedSymbol (',' DerivedSymbol) | ε
--  Call          : FuncName FuncCallArgs
--  Stm           : 'left'
--                | 'right'
--                | 'write' DerivedSymbol
--                | 'reject'
--                | 'accept'
--                | 'let' VarName '=' DerivedSymbol
--                | FuncDecl
--                | Call
--                | Stm '\n' Stm
--                | 'print'
--                | 'print' String
--                | 'while' Bexp '{' Stm '}'
--                | If

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["read", "True", "False", "not", "and", "or", "left",
                    "right", "write", "reject", "accept", "let", "if", "else",
                    "while", "print", "func"]

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

-- braces p = between openBrace closeBrace p' where
--     p'         = p <* whitespaceNewline
--     openBrace  = tok "{" <* optional newline
--     closeBrace = tok "}"

-- Parses a string encased in double quotes.
encasedString :: Parser String
encasedString = between (tok "\"") (tok "\"") (many (noneOf "\""))

-- Parses a tape symbol, the EBNF syntax of which is:
--  TapeSymbol  : LowerChar | UpperChar | Digit | ASCII-Symbol
tapeSymbol :: Parser TapeSymbol
tapeSymbol = noneOf "\'"

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

-- Parses a function argument, the EBNF syntax of which is:
--  ArgName : LowerChar (LowerChar | UpperChar | Digit)*
argName :: Parser ArgName
argName = identifier

-- Parses a derived symbol, the EBNF syntax of which is:
--  DerivedSymbol : 'read'
--                | VarName
--                | \' TapeSymbol \'
derivedSymbol :: Parser DerivedSymbol
derivedSymbol = Read <$ tok "read"
            <|> Var <$> varName
            <|> Literal <$> between (char '\'') (tok "\'") tapeSymbol

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
--       | DerivedSymbol '==' DerivedSymbol
--       | DerivedSymbol '<=' DerivedSymbol
bexp :: Parser Bexp
bexp = makeExprParser bexp' bexpOps

-- Parses if-else statement, the EBNF syntax of which is:
--  If     : 'if' { Stm } ElseIf
--  ElseIf : 'else if' { Stm } ElseIf | Else
--  Else   : 'else' { Stm } | ε
ifStm :: Parser Stm
ifStm = If <$ tok "if" <*> bexp <*> braces stmComp <*> many elseIfClause <*> elseClause where
    elseIfClause = do
        b <- tok "else if" *> bexp
        stmComp <- braces stmComp
        return (b, stmComp)
    elseClause = optional (tok "else" *> braces stmComp)

-- Parses argument names of a function declaration, the EBNF syntax of which is:
--  FuncDeclArgs : ArgName (',' ArgName)* | ε
funcDeclArgs :: Parser FuncDeclArgs
funcDeclArgs = argName `sepBy` whitespace

-- Parses a function declaration, the EBNF syntax of which is:
--  FuncDecl : 'func' FuncName FuncDeclArgs '{' Stm '}'
funcDecl :: Parser Stm
funcDecl = FuncDecl <$ tok "func" <*> funcName <*> funcDeclArgs <*> braces stmComp

-- Parses the arguments supplied to a function call, the EBNF syntax of which is:
--  FuncCallArgs : DerivedSymbol (',' DerivedSymbol) | ε
funcCallArgs :: Parser FuncCallArgs
funcCallArgs = derivedSymbol `sepBy` whitespace

-- Parses a function call, the EBNF syntax of which is:
--  Call : FuncName FuncCallArgs
funcCall :: Parser Stm
funcCall = Call <$> funcName <*> funcCallArgs

-- Parses the elements of the syntactic class Stm, except for composition.
stm' :: Parser Stm
stm' = try funcCall
   <|> MoveLeft <$ tok "left"
   <|> MoveRight <$ tok "right"
   <|> Write <$ tok "write" <*> derivedSymbol
   <|> Reject <$ tok "reject"
   <|> Accept <$ tok "accept"
   <|> VarDecl <$ tok "let" <*> varName <* tok "=" <*> derivedSymbol
   <|> funcDecl
   <|> try (PrintStr <$ tok "print" <*> encasedString)
   <|> try (PrintRead <$ tok "print")
   <|> While <$ tok "while" <*> bexp <*> braces stmComp
   <|> ifStm

-- Parses statements separated by newlines into a composition of statements.
stmComp :: Parser Stm
stmComp = (stms <* whitespaceNewline) >>= compose where
    stms :: Parser [Stm]
    stms = try ((:) <$> (stm' <* some (newline <* whitespace)) <*> stms)
       <|> (:) <$> stm' <*> pure []

    compose :: [Stm] -> Parser Stm
    compose []  = fail "Expected a statement"
    compose [x] = return x
    compose xs  = return (foldr1 Comp xs)

-- Parses a statement, the EBNF syntax of which is given below. The parser will
-- fail if not all input is consumed.
--  Stm : 'left'
--      | 'right'
--      | 'write' DerivedSymbol
--      | 'reject'
--      | 'accept'
--      | 'let' VarName '=' DerivedSymbol
--      | FuncDecl
--      | FuncName
--      | Stm '\n' Stm
--      | 'print'
--      | 'print' String
--      | 'while' Bexp '{' Stm '}'
--      | If
stm :: Parser Stm
stm = whitespaceNewline *> stmComp <* whitespaceNewline <* eof
