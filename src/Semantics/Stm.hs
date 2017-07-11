module Semantics.Stm (evalStm) where

import Control.Monad.Except hiding (fix)
import Data.Maybe (maybeToList)
import Semantics.Bexp
import Semantics.DerivedSymbol
import Semantics.Helpers
import State.App
import State.Config
import State.Error
import State.MachineClass
import Syntax.Tree

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: App Config -> App Config
evalLeft = fmap left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: App Config -> App Config
evalRight = fmap right

-- Evaluates writing to the tape.
evalWrite :: DerivedSymbol -> App Config -> App Config
evalWrite sym app = do
    val <- derivedSymbolVal sym app
    fmap (setCurr val) app

-- Evaluates an if-else statement.
evalIf :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> App Config -> App Config
evalIf bexp ifStm elseIfClauses elseStm = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b, block (evalStm stm))) allClauses
    allClauses = ((bexp, ifStm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
evalWhile :: Bexp -> Stm -> App Config -> App Config
evalWhile b body = fix f where
    f loop = cond [(bexpVal b, loop . (block (evalStm body)))]

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> App Config -> App Config
evalVarDecl name sym app = do
    val <- derivedSymbolVal sym app
    fmap (addVar name val) app

-- Evaluates a function declaration.
evalFuncDecl :: FuncName -> FuncDeclArgs -> Stm -> App Config -> App Config
evalFuncDecl name args body = fmap (addFunc name args body)

-- Checks that the number of arguments to a function is the same as the number
-- of arguments the function declaration specified.
checkNumArgs :: FuncName -> FuncDeclArgs -> FuncCallArgs -> App Config -> App Config
checkNumArgs name ds cs app | (length ds) == (length cs) = app
                            | otherwise                  = throwError err where
                                err = WrongNumArgs name ds cs

-- Evaluates the body of a function, after adding any arguments to the variable
-- environment. The variable and function environments are reset after executing
-- the body.
evalFuncBody :: FuncName -> FuncDeclArgs -> FuncCallArgs -> Stm -> App Config -> App Config
evalFuncBody name ds cs body = (block evalBody) . check where
    check    = checkNumArgs name ds cs
    evalBody = (evalStm body) . addedVarsP
    -- A `App Config` where the arguments have been added to the environment.
    addedVarsP app' = foldr (uncurry evalVarDecl) app' zippedArgs
    zippedArgs    = zip ds cs

-- Evaluates a function call.
evalCall :: FuncName -> FuncCallArgs -> App Config -> App Config
evalCall name args app = do
    config <- app
    let fMaybe = lookupFunc name config
    maybe err eval fMaybe where
        err                   = throwError (UndefFunc name)
        eval (argNames, body) = evalFuncBody name argNames args body app


-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> App Config -> App Config
evalComp stm1 stm2 = (evalStm stm2) . (evalStm stm1)

-- Evaluates printing the current symbol.
evalPrintRead :: App Config -> App Config
evalPrintRead app = do
    config <- app
    output [(getCurr config)] config

-- Evaluates printing a string.
evalPrintStr :: String -> App Config -> App Config
evalPrintStr str app = do
    config <- app
    output str config

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: Stm -> App Config -> App Config
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
evalStm (PrintRead)               = evalPrintRead
evalStm (PrintStr str)            = evalPrintStr str
