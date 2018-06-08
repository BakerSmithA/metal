module Syntax.Identifier where

import Syntax.Tree
import Syntax.Common
import qualified Syntax.Env as E

-- Parses a snake-case identifier.
snakeId :: Parser VarName
snakeId = (:) <$> (lowerChar <|> char '_') <*> many c where
    c = lowerChar <|> digitChar <|> char '_'

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["read", "True", "False", "not", "and", "or", "left",
                    "right", "write", "reject", "accept", "let", "if", "else",
                    "while", "print", "func", "import", "_printTape"]

-- Checks that the parsed identifier is not a reserved keyword.
reserveCheckedId :: Parser Identifier -> Parser Identifier
reserveCheckedId p = (p >>= check) <* lWhitespace where
        check word = if not (word `elem` reservedKeywords)
                        then return word
                        else fail $ "keyword " ++ show word ++ " cannot be an identifier"

-- Parses an identifier if the identifier has **not** already been declared.
newId :: Parser Identifier -> GetEnv a -> Parser Identifier
newId identifier getEnv = do
    i <- reserveCheckedId identifier
    env <- liftM getEnv get
    when (E.isTaken i env) (fail $ i ++ " already exists")
    return i

-- Parses an identifier if the identifier already exists, returning the new
-- id and its type.
refId :: Parser Identifier -> GetEnv a -> Parser (Identifier, a)
refId identifier getEnv = do
    i <- reserveCheckedId identifier
    env <- liftM getEnv get
    case E.get i env of
        Nothing     -> fail $ i ++ " does not exist"
        Just idType -> return (i, idType)

-- Parses an identifier if the identifier exists and has the expected type.
refExpTypedId :: (Eq a) => Parser Identifier -> a -> GetEnv a -> Parser Identifier
refExpTypedId identifier expectedType getEnv = do
    (i, idType) <- refId identifier getEnv
    when (expectedType /= idType) (fail "mistmatched types")
    return i

-- Parses an identifier if has not already been declared, and puts it in the
-- environment.
putNewId :: Parser Identifier -> a -> GetEnv a -> ModifyEnv a -> Parser Identifier
putNewId identifier newType getEnv modifyEnv = do
    i <- newId identifier getEnv
    modify (modifyEnv $ E.put i newType)
    return i

modifyNewId :: Parser Identifier -> (a -> a) -> GetEnv a -> ModifyEnv a -> Parser Identifier
modifyNewId identifier f getEnv modifyEnv = do
    i <- newId identifier getEnv
    modify (modifyEnv $ E.modify i f)
    return i
