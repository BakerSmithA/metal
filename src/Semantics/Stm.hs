module Semantics.Stm where

import Control.Monad.Except hiding (fix)
import Data.Maybe (maybeToList)
import Semantics.Bexp
import Semantics.DerivedSymbol
import Semantics.Helpers
import State.Config
import State.Error
import State.MachineClass
import Syntax.Tree

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: StateConfig -> StateConfig
evalLeft = fmap left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: StateConfig -> StateConfig
evalRight = fmap right

-- Evaluates writing to the tape.
evalWrite :: DerivedSymbol -> StateConfig -> StateConfig
evalWrite sym p = do
    val <- derivedSymbolVal sym p
    fmap (setCurr val) p

-- Evaluates an if-else statement.
evalIf :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> StateConfig -> StateConfig
evalIf bexp ifStm elseIfClauses elseStm = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b, block (evalStm stm))) allClauses
    allClauses = ((bexp, ifStm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
evalWhile :: Bexp -> Stm -> StateConfig -> StateConfig
evalWhile b body = fix f where
    f loop = cond [(bexpVal b, loop . (block (evalStm body)))]

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> StateConfig -> StateConfig
evalVarDecl name sym p = do
    val <- derivedSymbolVal sym p
    fmap (addVar name val) p

-- Evaluates a function declaration.
evalFuncDecl :: FuncName -> FuncDeclArgs -> Stm -> StateConfig -> StateConfig
evalFuncDecl name args body = fmap (addFunc name args body)

-- Checks that the number of arguments to a function is the same as the number
-- of arguments the function declaration specified.
checkNumArgs :: FuncName -> FuncDeclArgs -> FuncCallArgs -> StateConfig -> StateConfig
checkNumArgs name ds cs p | (length ds) == (length cs) = p
                          | otherwise                  = throwError err where
                              err = WrongNumArgs name ds cs

-- Evaluates the body of a function, after adding any arguments to the variable
-- environment. The variable and function environments are reset after executing
-- the body.
evalFuncBody :: FuncName -> FuncDeclArgs -> FuncCallArgs -> Stm -> StateConfig -> StateConfig
evalFuncBody name ds cs body = (block evalBody) . check where
    check    = checkNumArgs name ds cs
    evalBody = (evalStm body) . addedVarsP
    -- A `StateConfig` where the arguments have been added to the environment.
    addedVarsP p' = foldr (uncurry evalVarDecl) p' zippedArgs
    zippedArgs    = zip ds cs

-- Evaluates a function call.
evalCall :: FuncName -> FuncCallArgs -> StateConfig -> StateConfig
evalCall name args p = do
    config <- p
    let fMaybe = lookupFunc name config
    maybe err eval fMaybe where
        err                   = throwError (UndefFunc name)
        eval (argNames, body) = evalFuncBody name argNames args body p


-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> StateConfig -> StateConfig
evalComp stm1 stm2 = (evalStm stm2) . (evalStm stm1)

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: Stm -> StateConfig -> StateConfig
evalStm (MoveLeft)                = evalLeft
evalStm (MoveRight)               = evalRight
evalStm (Write sym)               = evalWrite sym
evalStm (Accept)                  = const accept
evalStm (Reject)                  = const reject
evalStm (If b stm elseIf elseStm) = evalIf b stm elseIf elseStm
evalStm (While b stm)             = evalWhile b stm
evalStm (VarDecl name sym)        = evalVarDecl name sym
evalStm (FuncDecl name args body) = evalFuncDecl name args body
evalStm (Call name args)          = evalCall name args
evalStm (Comp stm1 stm2)          = evalComp stm1 stm2
evalStm (PrintRead)               = id
evalStm (PrintStr _)              = id
