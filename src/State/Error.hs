module State.Error where

import Syntax.Tree
import Control.Exception

data RuntimeError = UndefVar VarName   -- An undefined variable was attempted to be used.
                  | UndefTape VarName -- An undefined tape was attempted to be used.
                  | UndefFunc FuncName -- An undefined function was attempted to be called.
                  | WrongNumArgs FuncName FuncDeclArgs FuncCallArgs -- The wrong number of arguments was supplied.
                  deriving (Eq)

instance Show RuntimeError where
    -- show :: RuntimeError -> String
    show (UndefVar  name)                 = "Undefined variable: " ++ name
    show (UndefTape name)                 = "Undefined tape: "     ++ name
    show (UndefFunc name)                 = "Undefined function: " ++ name
    show (WrongNumArgs name expected got) = "Wrong number of arguments supplied to: "
                                         ++ name ++ ", expected: " ++ (show expected)
                                         ++ " but got:" ++ (show got)

instance Exception RuntimeError
