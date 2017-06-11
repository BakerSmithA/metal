module Semantics.Denotational where

import Syntax.Tree
import State.Machine
import State.Config
import State.Env
import Control.Monad.Reader
import Data.Maybe

type ReaderState = ReaderT Env Machine Config

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> ReaderState -> ReaderT Env Machine TapeSymbol
derivedSymbolVal (Read)        r = mapReaderT getCurr r
derivedSymbolVal (Literal sym) r = return sym
derivedSymbolVal (Var name)    r = do
    -- TODO: Error Handling.
    Just val <- asks (lookupVar name)
    return val

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> ReaderState -> ReaderT Env Machine Bool
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
cond :: [(ReaderState -> ReaderT Env Machine Bool, ReaderState -> ReaderState)] -> (ReaderState -> ReaderState)
cond []                       r = r
cond ((predicate, branch):ps) r = do
    bVal <- predicate r
    if bVal then branch r
            else cond ps r

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: ReaderState -> ReaderState
evalLeft = mapReaderT left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: ReaderState -> ReaderState
evalRight = mapReaderT right

-- Evaluates writing to the tape.
evalWrite :: TapeSymbol -> ReaderState -> ReaderState
evalWrite sym = mapReaderT (setCurr sym)

-- Evaluates halting the machine by accepting.
evalAccept :: ReaderState -> ReaderState
evalAccept = mapReaderT (const HaltA)

-- Evaluates halting the machine by rejecting.
evalReject :: ReaderState -> ReaderState
evalReject = mapReaderT (const HaltR)

-- Evaluates an if-else statement.
evalIfElse :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> ReaderState -> ReaderState
evalIfElse b stm elseIfClauses elseStm = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b, evalS stm)) allClauses
    allClauses = ((b, stm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
-- evalWhile :: Bexp -> Stm -> ReaderState -> ReaderState
-- evalWhile = undefined

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> ReaderState -> ReaderState
evalVarDecl name sym r = do
    val <- derivedSymbolVal sym r
    local (addVar name val) r
--
-- -- Evaluates a function declaration.
-- evalFuncDecl :: FuncName -> Stm -> ReaderState -> ReaderState
-- evalFuncDecl =undefined
--
-- -- Evaluates a function call.
-- evalCall :: FuncName -> ReaderState -> ReaderState
-- evalCall = undefined
--
-- -- evalCall name config = do
-- --     -- TODO: Error handling.
-- --     Just body <- asks (lookupFunc name)
-- --     evalS body config
--
-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> ReaderState -> ReaderState
evalComp stm1 stm2 r = evalS stm1 r

--
-- -- Evaluates print the symbol under the read-write head.
-- evalPrintRead :: ReaderState -> ReaderState
-- evalPrintRead = undefined
--
-- -- Evaluates string an arbitrary string.
-- evalPrintStr :: String -> ReaderState -> ReaderState
-- evalPrintStr = undefined

-- Evalautes a statement in a configuration of a Turing machine.
evalS :: Stm -> ReaderState -> ReaderState
evalS = undefined
-- evalS (MoveLeft)            env = evalLeft
-- evalS (MoveRight)           env = evalRight
-- evalS (Write derSym)        env = \c -> evalWrite (derivedSymbolVal derSym env c) c
-- evalS (Reject)              env = \c -> evalReject
-- evalS (Accept)              env = \c -> evalAccept
-- evalS (If b stm elseif els) env = evalIfElse b stm elseif els env
-- evalS (While b stm)         env = evalWhile b stm env
-- evalS (VarDecl name derSym) env = evalVarDecl name derSym env
-- evalS (FuncDecl name body)  env = evalFuncDecl name body env
-- evalS (Call name)           env = evalCall name env
-- evalS (PrintRead)           env = evalPrintRead
-- evalS (PrintStr str)        env = evalPrintStr str
