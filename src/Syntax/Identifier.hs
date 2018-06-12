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

-- Parses an identifier if the identifier already exists, returning the new
-- id and its type.
refId :: ParserM Identifier -> ParserM (Identifier, EnvDecl)
refId p = do
    i <- p
    idType <- getM i
    return (i, idType)

-- Checks that the value returned by p satisfies the type check predicate.
expType :: ParserM t -> (t -> Bool) -> ParserM t
expType p check = do
    x <- p
    if check x
        then return x
        else (fail "Mismatched types")

-- Checks that an identifier has the given type.
expTypeId :: ParserM (Identifier, t) -> (t -> Bool) -> ParserM Identifier
expTypeId p check = fmap fst (expType p (check . snd))

-- Checks that the data type of the result is the expected type.
expDataType :: (Typed t) => ParserM t -> DataType -> ParserM t
expDataType p ex = expType p (\x -> typeOf x == ex)
