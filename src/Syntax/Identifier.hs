module Syntax.Identifier where

import Syntax.Tree
import Syntax.Common
import Debug.Trace
import Control.Monad.State.Lazy (get)

-- Parses a snake-case identifier.
snakeId :: ParserM VarName
snakeId = (:) <$> (lowerChar <|> char '_') <*> many c where
    c = lowerChar <|> digitChar <|> char '_'

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["read", "True", "False", "not", "and", "or", "left",
                    "right", "write", "reject", "accept", "let", "if", "else",
                    "while", "print", "func", "import", "_printTape"]

-- Checks that the parsed identifier is not a reserved keyword.
reserveCheckedId :: ParserM Identifier -> ParserM Identifier
reserveCheckedId p = (p >>= check) <* lWhitespace where
        check word = if not (word `elem` reservedKeywords)
                        then return word
                        else fail $ "keyword " ++ show word ++ " cannot be an identifier"

-- Parses an identifier if the identifier has **not** already been declared.
newId :: ParserM Identifier -> ParserM Identifier
newId p = do
    i <- reserveCheckedId p
    taken <- isTakenM i
    s <- get
    trace (i ++ ", " ++ (show s) ++ ", " ++ (show taken)) $ if not taken
        then return i
        else (fail $ i ++ " already exists")

-- Parses an identifier if the identifier already exists, returning the new
-- id and its type.
refId :: ParserM Identifier -> ParserM (Identifier, EnvDecl)
refId p = do
    i <- reserveCheckedId p
    idType <- getM i
    case idType of
        Nothing -> fail $ i ++ " does not exist"
        Just t -> return (i, t)

-- Parses an identifier if the identifier exists and has the expected type.
refExpTypeId :: ParserM Identifier -> EnvDecl -> ParserM Identifier
refExpTypeId p expType = do
    (i, idType) <- refId p
    if expType == idType
        then return i
        else (fail "mistmatched types")

-- Parses an identifier if has not already been declared, and puts it in the
-- environment.
putNewId :: ParserM Identifier -> EnvDecl -> ParserM Identifier
putNewId p idType = do
    i <- newId p
    putM i idType
    return i
