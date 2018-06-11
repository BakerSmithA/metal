module Syntax.Identifier where

import Syntax.Tree
import Syntax.Common

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["read", "True", "False", "not", "and", "or", "left",
                    "right", "write", "reject", "accept", "let", "if", "else",
                    "while", "print", "func", "import", "_printTape", "struct"]

-- Checks that the parsed identifier is not a reserved keyword.
reserveCheckedId :: ParserM Identifier -> ParserM Identifier
reserveCheckedId p = (p >>= check) <* lWhitespace where
        check word = if not (word `elem` reservedKeywords)
                        then return word
                        else fail $ "keyword " ++ show word ++ " cannot be an identifier"

identifier :: ParserM Char -> ParserM Char -> ParserM Identifier
identifier start body = reserveCheckedId p where
    p = (:) <$> start <*> many body

-- Parses a snakecase identifier.
snakeId :: ParserM Identifier
snakeId = identifier start body where
    start = lowerChar <|> char '_'
    body = start <|> digitChar

-- Parses a camel-case identifier.
camelId :: ParserM Identifier
camelId = identifier start body where
    start = upperChar
    body = start <|> lowerChar <|> digitChar

-- Parses an identifier if the identifier has **not** already been declared.
newId :: ParserM Identifier -> ParserM Identifier
newId p = do
    i <- p
    taken <- isTakenM i
    if not taken
        then return i
        else (fail $ i ++ " already exists")

-- Uses getType to retrieve the type of the identifier, if it exists in the
-- environment.
refIdWith :: ParserM Identifier -> (Identifier -> ParserM EnvDecl) -> ParserM (Identifier, EnvDecl)
refIdWith p getType = do
    i <- p
    idType <- getType i
    return (i, idType)

-- Parses an identifier if the identifier already exists, returning the new
-- id and its type.
refId :: ParserM Identifier -> ParserM (Identifier, EnvDecl)
refId p = refIdWith p getM

-- Parses an identifier if the identifier exists and has the expected type.
expTypeId :: ParserM Identifier -> EnvDecl -> ParserM Identifier
expTypeId p expType = do
    (i, idType) <- refId p
    if expType == idType
        then return i
        else (fail "mistmatched types")
