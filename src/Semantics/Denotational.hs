module Semantics.Denotational where

import Syntax.Tree
import State.Config
import State.Machine
import State.Program

type ProgConfig = Prog Config

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> Prog TapeSymbol
derivedSymbolVal (Read)        = undefined
derivedSymbolVal (Var name)    = undefined
derivedSymbolVal (Literal sym) = undefined

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> Prog Bool
bexpVal = undefined

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(ProgConfig -> Prog Bool, ProgConfig -> ProgConfig)] -> (ProgConfig -> ProgConfig)
cond = undefined

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: ProgConfig -> ProgConfig
evalLeft = fmap left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: ProgConfig -> ProgConfig
evalRight = fmap right

-- Evaluates writing to the tape.
evalWrite :: DerivedSymbol -> ProgConfig -> ProgConfig
evalWrite sym p = do
    val <- derivedSymbolVal sym
    fmap (setCurr val) p

-- Evaluates halting the machine by accepting.
evalAccept :: ProgConfig
evalAccept = undefined

-- Evaluates halting the machine by rejecting.
evalReject :: ProgConfig
evalReject = undefined

-- Evaluates an if-else statement.
evalIf :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> ProgConfig -> ProgConfig
evalIf = undefined

-- Evaluates a while loop.
evalWhile :: Bexp -> Stm -> ProgConfig -> ProgConfig
evalWhile = undefined

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> ProgConfig -> ProgConfig
evalVarDecl = undefined

-- Evaluates a function declaration.
evalFuncDecl :: FuncName -> Stm -> ProgConfig -> ProgConfig
evalFuncDecl = undefined

-- Evaluates a function call.
evalCall :: FuncName -> ProgConfig -> ProgConfig
evalCall = undefined

-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> ProgConfig -> ProgConfig
evalComp = undefined

-- Evaluates print the symbol under the read-write head.
-- TODO: Add I/O.
evalPrintRead :: ProgConfig -> ProgConfig
evalPrintRead = undefined

-- Evaluates string an arbitrary string.
-- TODO: Add I/O.
evalPrintStr :: String -> ProgConfig -> ProgConfig
evalPrintStr = undefined

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: Stm -> ProgConfig -> ProgConfig
evalStm (MoveLeft)                = evalLeft
evalStm (MoveRight)               = evalRight
evalStm (Write sym)               = evalWrite sym
evalStm (Accept)                  = const evalAccept
evalStm (Reject)                  = const evalReject
evalStm (If b stm elseIf elseStm) = evalIf b stm elseIf elseStm
evalStm (While b stm)             = evalWhile b stm
evalStm (VarDecl name sym)        = evalVarDecl name sym
evalStm (FuncDecl name body)      = evalFuncDecl name body
evalStm (Call name)               = evalCall name
evalStm (PrintRead)               = evalPrintRead
evalStm (PrintStr str)            = evalPrintStr str
