module State.Error where

import Syntax.Tree

data RuntimeError = UndefVar VarName   -- An undefined function was attempted to be called.
                  | UndefFunc FuncName -- An undefined variable was attempted to be used.
                  deriving (Eq)

instance Show RuntimeError where
    -- show :: RuntimeError -> String
    show (UndefVar  name) = "Undefined variable: " ++ name
    show (UndefFunc name) = "Undefined function: " ++ name
