module Syntax.Func where

import Syntax.Common
import Syntax.Identifier
import Syntax.VariableExpr

-- Attempts to parse an identifier used to declare a new function. Does **not**
-- add the function to the environment if it does not exist. Fails if the
-- function already exists. EBNF:
--  FuncName : LowerChar (LowerChar | UpperChar | Digit)*
newFuncId :: ParserM FuncName
newFuncId = newId snakeId

-- Attempts to use a declared variable, but does **not** check for matching
-- types. If the variable does not exist then parsing fails. EBNF:
--  FuncName : LowerChar (LowerChar | UpperChar | Digit)*
refFuncId :: ParserM (FuncName, [DataType])
refFuncId = do
    (name, idType) <- refId snakeId
    case idType of
        PFunc argTypes -> return (name, argTypes)
        _              -> fail "Expected function"

-- Parses a function argument, the EBNF syntax of which is:
--  ArgName : LowerChar (LowerChar | UpperChar | Digit)*
newArgId :: ParserM ArgName
newArgId = newId snakeId

-- Parses an argument to a function, the EBNF of which is the same as a TypedVar.
funcDeclArg :: ParserM FuncDeclArg
funcDeclArg = do
    (name, argType) <- typeAnnotated newArgId
    putM name (PVar argType)
    return (name, argType)

-- Parses argument names of a function declaration, the EBNF syntax of which is:
--  FuncDeclArgs  : FuncDeclArg (' ' TypedVar)* | ε
funcDeclArgs :: ParserM [FuncDeclArg]
funcDeclArgs = funcDeclArg `sepBy` lWhitespace

-- Adds the function to the environment.
putFunc :: FuncName -> [FuncDeclArg] -> ParserM ()
putFunc name args = putM name (PFunc argTypes) where
    argTypes = map argType args

-- Parses the arguments and body of a function.
funcArgsBody :: FuncName -> ParserM Stm -> ParserM ([FuncDeclArg], Stm)
funcArgsBody name stm = do
    args <- funcDeclArgs
    -- Expose the function inside the function itself, allowing recursion.
    putFunc name args
    body <- block (braces stm)
    return (args, body)

-- Parses a function declaration, the EBNF syntax of which is:
--  FuncDecl : 'func' FuncName FuncDeclArgs '{' Stm '}'
funcDecl :: ParserM Stm -> ParserM Stm
funcDecl stm = do
    _ <- lTok "func"
    name <- newFuncId
    (args, body) <- block (funcArgsBody name stm)

    putFunc name args

    return (FuncDecl name args body)

-- Parses the arguments supplied to a function call, the EBNF syntax of which is:
--  FuncCallArgs : FuncCallArg (',' FuncCallArg) | ε
funcCallArgs :: [DataType] -> ParserM [FuncCallArg]
funcCallArgs = matchedTypes (\t -> expAnyValExpr t <|> parens (expAnyValExpr t))

-- Parses a function call, the EBNF syntax of which is:
--  Call : FuncName FuncCallArgs
funcCall :: ParserM Stm
funcCall = do
    (name, expectedArgTypes) <- refFuncId
    args <- funcCallArgs expectedArgTypes
    return (Call name args)
