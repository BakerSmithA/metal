module Semantics.Program where

-- Evaluates a program by importing any files, then evaluating the statement.
evalProgram :: Program -> AppConfig -> AppConfig
evalProgram (Program imports stm) = evalStm stm
