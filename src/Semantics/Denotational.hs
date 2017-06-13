module Semantics.Denotational where

import Syntax.Tree
import State.Machine
import State.Config
import State.Env
import Semantics.Helpers
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

type StateM a     = ReaderT Env Machine a
type StateMConfig = StateM Config

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> StateMConfig -> StateM TapeSymbol
derivedSymbolVal (Read)        r = mapReaderT getCurr r
derivedSymbolVal (Literal sym) r = return sym
derivedSymbolVal (Var name)    r = do
    -- TODO: Error Handling.
    Just val <- asks (lookupVar name)
    return val

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> StateMConfig -> StateM Bool
bexpVal (TRUE)      r = return True
bexpVal (FALSE)     r = return False
bexpVal (Not b)     r = liftM not (bexpVal b r)
bexpVal (And b1 b2) r = liftM2 (&&) (bexpVal b1 r) (bexpVal b2 r)
bexpVal (Or b1 b2)  r = liftM2 (||) (bexpVal b1 r) (bexpVal b2 r)
bexpVal (Eq s1 s2)  r = liftM2 (==) (derivedSymbolVal s1 r) (derivedSymbolVal s2 r)
bexpVal (Le s1 s2)  r = liftM2 (<=) (derivedSymbolVal s1 r) (derivedSymbolVal s2 r)

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(StateMConfig -> StateM Bool, StateMConfig -> StateMConfig)] -> (StateMConfig -> StateMConfig)
cond []                       r = r
cond ((predicate, branch):ps) r = do
    bVal <- predicate r
    if bVal then branch r
            else cond ps r

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: StateMConfig -> StateMConfig
evalLeft = mapReaderT left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: StateMConfig -> StateMConfig
evalRight = mapReaderT right

-- Evaluates writing to the tape.
evalWrite :: StateM TapeSymbol -> StateMConfig -> StateMConfig
evalWrite sym r = do
    val <- sym
    mapReaderT (setCurr val) r

-- Evaluates halting the machine by accepting.
evalAccept :: StateMConfig
evalAccept = lift HaltA

-- Evaluates halting the machine by rejecting.
evalReject :: StateMConfig
evalReject = lift HaltR

-- Evaluates an if-else statement.
evalIf :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> StateMConfig -> StateMConfig
evalIf b stm elseIfClauses elseStm = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b, evalStm stm)) allClauses
    allClauses = ((b, stm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
evalWhile :: Bexp -> Stm -> StateMConfig -> StateMConfig
evalWhile b stm = fix f where
    f loop = cond [(bexpVal b, (evalStm stm) . loop)]

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> StateMConfig -> StateMConfig
evalVarDecl name sym r = do
    val <- derivedSymbolVal sym r
    local (addVar name val) r

-- Evaluates a function declaration.
evalFuncDecl :: FuncName -> Stm -> StateMConfig -> StateMConfig
evalFuncDecl name body = local (addFunc name body)

-- Evaluates a function call.
evalCall :: FuncName -> StateMConfig -> StateMConfig
evalCall name r = do
    -- TODO: Error handling.
    Just body <- asks (lookupFunc name)
    evalStm body r

-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> StateMConfig -> StateMConfig
evalComp stm1 stm2 = (evalStm stm1) . (evalStm stm2)

-- Evaluates print the symbol under the read-write head.
-- TODO: Add I/O.
evalPrintRead :: StateMConfig -> StateMConfig
evalPrintRead = id

-- Evaluates string an arbitrary string.
-- TODO: Add I/O.
evalPrintStr :: String -> StateMConfig -> StateMConfig
evalPrintStr str = id

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: Stm -> StateMConfig -> StateMConfig
evalStm (MoveLeft)                = evalLeft
evalStm (MoveRight)               = evalRight
evalStm (Write sym)               = both evalWrite (derivedSymbolVal sym)
evalStm (Accept)                  = const evalAccept
evalStm (Reject)                  = const evalReject
evalStm (If b stm elseIf elseStm) = evalIf b stm elseIf elseStm
evalStm (While b stm)             = evalWhile b stm
evalStm (VarDecl name sym)        = evalVarDecl name sym
evalStm (FuncDecl name body)      = evalFuncDecl name body
evalStm (Call name)               = evalCall name
evalStm (PrintRead)               = evalPrintRead
evalStm (PrintStr str)            = evalPrintStr str
