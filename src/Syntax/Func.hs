module Syntax.Func where

import Syntax.Helper
import Syntax.Env as E
import Syntax.Identifier
import Syntax.Variable

-- Argument to a function, the EBNF is:
--  FuncDeclArg : ArgName ':' FuncArgType
funcDeclArg :: Parser FuncDeclArg
funcDeclArg = do
    name <- newArg
    _ <- lTok ":"
    argType <- dataType
    modify (modifyVarEnv (E.put name argType))
    return (FuncDeclArg name argType)

-- Parses argument names of a function declaration, the EBNF syntax of which is:
--  FuncDeclArgs : FuncDeclArg (' ' FuncDeclArg)* | ε
funcDeclArgs :: Parser FuncDeclArgs
funcDeclArgs = funcDeclArg `sepBy` lWhitespace

-- Parses the arguments and body of a function.
funcArgsBody :: Parser Stm -> Parser (FuncDeclArgs, Stm)
funcArgsBody stm = do
    args <- funcDeclArgs
    body <- braces stm
    return (args, body)

-- Parses a function declaration, the EBNF syntax of which is:
--  FuncDecl : 'func' FuncName FuncDeclArgs '{' Stm '}'
funcDecl :: Parser Stm -> Parser Stm
funcDecl stm = do
    _ <- lTok "func"
    name <- newFunc
    (args, body) <- block (funcArgsBody stm)

    let argTypes = map (\(FuncDeclArg _ argType) -> argType) args
    modify (modifyFuncEnv (E.put name argTypes))

    return (FuncDecl name args body)

-- Parses an argument to a function call, the EBNF syntax of which is:
--  FuncCallArg : DerivedValue | TapeLiteral
funcCallArg :: DataType -> Parser FuncCallArg
funcCallArg expectedType = arg <* lWhitespace where
    arg = Derived <$> derivedSymbol expectedType
      <|> TapeLiteral <$> tapeLiteral

-- Parses the arguments supplied to a function call, the EBNF syntax of which is:
--  FuncCallArgs : FuncCallArg (',' FuncCallArg) | ε
funcCallArgs :: [DataType] -> Parser FuncCallArgs
funcCallArgs expectedTypes = foldl combine (return []) parseArgs where
    combine acc arg = (++) <$> acc <*> (fmap (\x -> [x]) arg)
    parseArgs = zipWith (\x y -> x y) (repeat funcCallArg) expectedTypes

-- Parses a function call, the EBNF syntax of which is:
--  Call : FuncName FuncCallArgs
funcCall :: Parser Stm
funcCall = do
    (name, expectedArgTypes) <- refFunc
    args <- funcCallArgs expectedArgTypes
    return (Call name args)
