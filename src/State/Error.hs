module State.Error where

import Syntax.Tree
import Control.Exception

data RuntimeError = UndefVar
                  | UndefFunc FuncName -- An undefined function was attempted to be called.
                  | WrongNumArgs FuncName [FuncDeclArg] [FuncCallArg] -- The wrong number of arguments was supplied.
                  | MismatchedTypes VarName FuncName DataType FuncCallArg
                  deriving (Eq)

instance Show RuntimeError where
    -- show :: RuntimeError -> String
    show (UndefVar)                        = "Undefined variable"
    show (UndefFunc name)                  = "Undefined function: " ++ name
    show (WrongNumArgs fName expected got) =
        "Wrong number of arguments supplied to '" ++ fName ++ "'.\n"
     ++ "Expected " ++ (show expected) ++ " but got " ++ (show got)
    show (MismatchedTypes varName fName expected got) =
        "Mistmached type for '" ++ varName ++ "' when calling '" ++ fName ++ "'.\n"
     ++ "Expected type '" ++ (show expected) ++ "' but got '" ++ (show got)

instance Exception RuntimeError
