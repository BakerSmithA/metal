module Semantics.Program where

-- Evaluates a program by importing any files, then evaluating the statement.
evalProgram :: Program -> StateConfig -> StateConfig
evalProgram (Program imports stm) = evalStm stm
