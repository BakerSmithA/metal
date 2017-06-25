module Semantics.Denotational where

import Control.Monad.Except hiding (fix)
import Control.Monad.Reader hiding (fix)
import Data.Maybe (maybeToList)
import State.Config
import State.Env
import State.Error
import State.MachineClass
import State.Program
import State.Trans.Machine
import Syntax.Tree
import System.IO

-- Fixpoint operator used to defined loops.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(ProgConfig -> Prog Bool, ProgConfig -> ProgConfig)] -> (ProgConfig -> ProgConfig)
cond []                       p = p
cond ((predicate, branch):ps) p = do
    bVal <- predicate p
    if bVal then branch p
            else cond ps p

-- Retrieves the value of a variable, throwing an undefined variable error if
-- the variable has not be defined.
getVarVal :: VarName -> Prog TapeSymbol
getVarVal name = do
    val <- asks (lookupVar name)
    maybe (throwError (UndefVar name)) return val

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> ProgConfig -> Prog TapeSymbol
derivedSymbolVal (Read)        p = fmap getCurr p
derivedSymbolVal (Literal sym) _ = return sym
derivedSymbolVal (Var name)    _ = getVarVal name

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> ProgConfig -> Prog Bool
bexpVal (TRUE)      _ = return True
bexpVal (FALSE)     _ = return False
bexpVal (Not b)     p = liftM not (bexpVal b p)
bexpVal (And b1 b2) p = liftM2 (&&) (bexpVal b1 p) (bexpVal b2 p)
bexpVal (Or b1 b2)  p = liftM2 (||) (bexpVal b1 p) (bexpVal b2 p)
bexpVal (Eq s1 s2)  p = liftM2 (==) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)
bexpVal (Le s1 s2)  p = liftM2 (<=) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: ProgConfig -> ProgConfig
evalLeft = fmap left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: ProgConfig -> ProgConfig
evalRight = fmap right

-- Evaluates writing to the tape.
evalWrite :: DerivedSymbol -> ProgConfig -> ProgConfig
evalWrite sym p = do
    val <- derivedSymbolVal sym p
    fmap (setCurr val) p

-- Evaluates an if-else statement.
evalIf :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> ProgConfig -> ProgConfig
evalIf bexp ifStm elseIfClauses elseStm = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b, evalStm stm)) allClauses
    allClauses = ((bexp, ifStm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
evalWhile :: Bexp -> Stm -> ProgConfig -> ProgConfig
evalWhile b body = fix f where
    f loop = cond [(bexpVal b, loop . (evalStm body))]

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> ProgConfig -> ProgConfig
evalVarDecl name sym p = do
    val <- derivedSymbolVal sym p
    local (addVar name val) p

-- Evaluates a function declaration.
evalFuncDecl :: FuncName -> Stm -> ProgConfig -> ProgConfig
evalFuncDecl name body = local (addFunc name body)

-- Evaluates a function call.
evalCall :: FuncName -> ProgConfig -> ProgConfig
evalCall name p = do
    body <- asks (lookupFunc name)
    maybe err eval body where
        err      = throwError (UndefFunc name)
        eval stm = evalStm stm p

-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> ProgConfig -> ProgConfig
evalComp stm1 stm2 = (evalStm stm2) . (evalStm stm1)

-- Evaluates print the symbol under the read-write head.
evalPrintRead :: ProgConfig -> ProgConfig
evalPrintRead = id

-- evalPrintRead p = do
--     config <- p
--     (liftIO . putStrLn . show . getCurr) config

-- Evaluates string an arbitrary string.
-- evalPrintStr :: String -> Prog ()
-- evalPrintStr str = liftIO (putStrLn str)

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: Stm -> ProgConfig -> ProgConfig
evalStm (MoveLeft)                = evalLeft
evalStm (MoveRight)               = evalRight
evalStm (Write sym)               = evalWrite sym
evalStm (Accept)                  = const accept
evalStm (Reject)                  = const reject
evalStm (If b stm elseIf elseStm) = evalIf b stm elseIf elseStm
evalStm (While b stm)             = evalWhile b stm
evalStm (VarDecl name sym)        = evalVarDecl name sym
evalStm (FuncDecl name body)      = evalFuncDecl name body
evalStm (Call name)               = evalCall name
evalStm (Comp stm1 stm2)          = evalComp stm1 stm2
evalStm (PrintRead)               = id
evalStm (PrintStr str)            = id
