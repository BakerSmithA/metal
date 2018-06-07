module Syntax.Identifier where

import Syntax.Tree
import Syntax.Helper
import qualified Syntax.Env as E

-- The keywords reserved by the language. These are not allowed to be function
-- names, however function names are allowed to contain reserved keywords.
reservedKeywords :: [String]
reservedKeywords = ["read", "True", "False", "not", "and", "or", "left",
                    "right", "write", "reject", "accept", "let", "if", "else",
                    "while", "print", "func", "import", "_printTape"]

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

-- Parses an identifier if the identifier has **not** already been declared.
newId :: GetEnv a -> Parser Identifier
newId getEnv = do
    i <- identifier
    env <- liftM getEnv get
    when (E.isTaken i env) (fail $ i ++ " already exists")
    return i

-- Parses an identifier if the identifier already exists, returning the new
-- id and its type.
refId :: GetEnv a -> Parser (Identifier, a)
refId getEnv = do
    i <- identifier
    env <- liftM getEnv get
    case E.get i env of
        Nothing     -> fail $ i ++ " does not exist"
        Just idType -> return (i, idType)

-- Parses an identifier if the identifier exists and has the expected type.
refTypedId :: (Eq a) => a -> GetEnv a -> Parser Identifier
refTypedId expectedType getEnv = do
    (i, idType) <- refId getEnv
    when (expectedType /= idType) (fail "mistmatched types")
    return i

-- Parses an identifier if has not already been declared, and puts it in the
-- environment.
putNewId :: a -> GetEnv a -> ModifyEnv a -> Parser Identifier
putNewId newType getEnv modifyEnv = do
    i <- newId getEnv
    modify (modifyEnv $ E.put i newType)
    return i
