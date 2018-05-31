module State.Error where

import Syntax.Tree
import Control.Exception

data RuntimeError = UndefVar VarName   -- An undefined variable was attempted to be used.
                  | UndefTape VarName -- An undefined tape was attempted to be used.
                  | UndefFunc FuncName -- An undefined function was attempted to be called.
                  | WrongNumArgs FuncName FuncDeclArgs FuncCallArgs -- The wrong number of arguments was supplied.
                  | MismatchedTypes VarName FuncName DataType DerivedValue
                  deriving (Eq)

instance Show RuntimeError where
    -- show :: RuntimeError -> String
    show (UndefVar  name)                  = "Undefined variable: " ++ name
    show (UndefTape name)                  = "Undefined tape: "     ++ name
    show (UndefFunc name)                  = "Undefined function: " ++ name
    show (WrongNumArgs fName expected got) =
        "Wrong number of arguments supplied to '" ++ fName ++ "'.\n"
     ++ "Expected " ++ (show expected) ++ " but got " ++ (show got)
    show (MismatchedTypes varName fName expected got) =
        "Mistmached type for '" ++ varName ++ "' when calling '" ++ fName ++ "'.\n"
     ++ "Expected type '" ++ (show expected) ++ "' but got '" ++ (show got)

instance Exception RuntimeError
