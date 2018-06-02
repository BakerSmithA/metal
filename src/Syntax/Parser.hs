module Syntax.Parser where

import Control.Monad (void)
import Control.Monad.State.Lazy (StateT, evalStateT, get, put, lift)
import Syntax.Tree
import Syntax.ParseState as S
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.String as M

type Parser = StateT ParseState M.Parser

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
--  Imports       : ('import ' String '\n'+)*
--  Program       : Imports Stm

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
whitespaceConsumer :: M.Parser Char -> M.Parser ()
whitespaceConsumer sc = L.space (void sc) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//" <* void (many newline)
        blockCmnt = L.skipBlockComment "/*" "*/" <* void (many newline)

-- Comsumes whitespace and comments, but not newlines.
whitespace :: M.Parser ()
whitespace = whitespaceConsumer (oneOf "\t ")

lWhitespace :: Parser ()
lWhitespace = lift whitespace

whitespaceNewline :: M.Parser ()
whitespaceNewline = whitespaceConsumer spaceChar

-- Consumes whitespace, comments, and newlines.
lWhitespaceNewline :: Parser ()
lWhitespaceNewline = lift whitespaceNewline

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
tok :: String -> M.Parser String
tok s = string s <* whitespace

-- Succeeds if the specified string can be parsed, followed by any ignored
-- whitespace or comments.
lTok :: String -> Parser String
lTok = lift . tok

-- Parses a string enclosed in parenthesis.
parens :: Parser a -> Parser a
parens = between (lTok "(") (lTok ")")

-- Parses a string enclosed in curly braces.
braces :: Parser a -> Parser a
braces = between (lTok "{" <* lWhitespaceNewline) (lTok "}")

quoted :: Parser a -> Parser a
quoted = between (lTok "\"") (lTok "\"")

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
identifier = (str >>= check) <* lWhitespace where
    str        = (:) <$> lowerChar <*> many alphaNumChar
    check word = if not (word `elem` reservedKeywords)
                    then return word
                    else fail $ "keyword " ++ show word ++ " cannot be an identifier"

-- Attempts to parse an identifier used to declare a new variable.
-- Fails if the variable already exists.
-- EBNF:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
varDeclId :: Parser String
varDeclId = do
    state <- get
    varId <- identifier
    if not (S.isTaken varId state)
        then do
            put (S.putVar varId state)
            return varId
        else
            fail $ "variable " ++ show varId ++ " already declared"

-- Attempts to use a declared variable. If the variable does not exist then
-- parsing fails.
-- EBNF:
--  VarName : LowerChar (LowerChar | UpperChar | Digit)*
varUseId :: Parser String
varUseId = do
    state <- get
    varId <- identifier
    if S.canRef varId state
        then return varId
        else fail $ "variable " ++ show varId ++ " not declared"

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
derivedSymbol = Read <$ lTok "read" <* lWhitespace <*> varUseId
            <|> Var <$> varUseId
            <|> Literal <$> between (char '\'') (lTok "\'") tapeSymbol
            <|> parens derivedSymbol

-- Parses the basis elements of the boolean expressions, plus boolean
-- expressions wrapped in parenthesis.
bexp' :: Parser Bexp
bexp' = try (parens bexp)
    <|> TRUE  <$ lTok "True"
    <|> FALSE <$ lTok "False"
    <|> try (Eq <$> derivedSymbol <* lTok "==" <*> derivedSymbol)
    <|> try (Le <$> derivedSymbol <* lTok "<=" <*> derivedSymbol)
    <|> try (Ne <$> derivedSymbol <* lTok "!=" <*> derivedSymbol)

-- The operators that can work on boolean expressions. There is no precedence,
-- instead the expression is evaualted from left to right.
bexpOps :: [[Operator Parser Bexp]]
bexpOps = [[Prefix (Not <$ lTok "not")],
           [InfixL (And <$ lTok "and"), InfixL (Or <$ lTok "or")]]

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
ifStm = If <$ lTok "if" <*> bexp <*> braces stmComp <*> many elseIfClause <*> elseClause where
    elseIfClause = do
        bool <- lTok "else if" *> bexp
        statement <- braces stmComp
        return (bool, statement)
    elseClause = optional (lTok "else" *> braces stmComp)

-- Type of data passed to a function, the EBNF of which is:
--  FuncArgType   : 'Tape' | 'Sym'
dataType :: Parser DataType
dataType = SymType <$ lTok "Sym"
       <|> TapeType <$ lTok "Tape"

-- Argument to a function, the EBNF is:
--  FuncDeclArg : ArgName ':' FuncArgType
funcDeclArg :: Parser FuncDeclArg
funcDeclArg = FuncDeclArg <$> argName <* lTok ":" <*> dataType

-- Parses argument names of a function declaration, the EBNF syntax of which is:
--  FuncDeclArgs : FuncDeclArg (' ' FuncDeclArg)* | ε
funcDeclArgs :: Parser FuncDeclArgs
funcDeclArgs = funcDeclArg `sepBy` lWhitespace

-- Parses a function declaration, the EBNF syntax of which is:
--  FuncDecl : 'func' FuncName FuncDeclArgs '{' Stm '}'
funcDecl :: Parser Stm
funcDecl = FuncDecl <$ lTok "func" <*> funcName <*> funcDeclArgs <*> body where
    body = do
        -- Variable names can be overwritten inside functions.
        currState <- get
        put (descendScope currState)
        parsedBody <- braces stmComp
        put currState
        return parsedBody

-- Parsers the contents of a tape literal, e.g. "abcd"
tapeLiteral :: Parser String
tapeLiteral = quoted (many tapeSymbol)

-- Parses an argument to a function call, the EBNF syntax of which is:
--  FuncCallArg : DerivedValue | TapeLiteral
funcCallArg :: Parser FuncCallArg
funcCallArg = Derived <$> derivedSymbol
          <|> TapeLiteral <$> tapeLiteral

-- Parses the arguments supplied to a function call, the EBNF syntax of which is:
--  FuncCallArgs : FuncCallArg (',' FuncCallArg) | ε
funcCallArgs :: Parser FuncCallArgs
funcCallArgs = funcCallArg `sepBy` lWhitespace

-- Parses a function call, the EBNF syntax of which is:
--  Call : FuncName FuncCallArgs
funcCall :: Parser Stm
funcCall = Call <$> funcName <*> funcCallArgs

-- Parses the elements of the syntactic class Stm, except for composition.
stm' :: Parser Stm
stm' = try funcCall
   <|> MoveLeft <$ lTok "left" <* lWhitespace <*> varUseId
   <|> MoveRight <$ lTok "right" <* lWhitespace <*> varUseId
   <|> try (Write <$ lTok "write" <*> varUseId <* lWhitespace <*> derivedSymbol)
   <|> WriteStr <$ lTok "write" <*> varUseId <* lWhitespace <*> quotedString
   <|> Reject <$ lTok "reject"
   <|> Accept <$ lTok "accept"
   <|> try (VarDecl <$ lTok "let" <*> varDeclId <* lTok "=" <*> derivedSymbol)
   <|> TapeDecl <$ lTok "let" <*> varDeclId <* lTok "=" <*> tapeLiteral
   <|> funcDecl
   <|> try (PrintStr <$ lTok "print" <*> quotedString)
   <|> try (PrintRead <$ lTok "print" <* lWhitespace <*> varUseId)
   <|> DebugPrintTape <$ lTok "_printTape" <*> varUseId
   <|> While <$ lTok "while" <*> bexp <*> braces stmComp
   <|> ifStm

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
program :: Parser Program
program = Program <$ lift importPaths <*> stm <* eof

parseState :: ParseState -> Parser a -> String -> String -> Either (ParseError (Token String) Dec) a
parseState initialState p src s = parse (evalStateT p initialState) src s

parseM :: Parser a -> String -> String -> Either (ParseError (Token String) Dec) a
parseM = parseState S.empty
