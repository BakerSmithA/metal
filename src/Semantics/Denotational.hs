module Semantics.Denotational where

import Control.Monad.Except hiding (fix)
import Data.Maybe (maybeToList)
import State.Config
import State.Error
import State.MachineClass
import State.App
import Syntax.Tree

type AppConfig = App Config

-- Fixpoint operator used to defined loops.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(AppConfig -> App Bool, AppConfig -> AppConfig)] -> (AppConfig -> AppConfig)
cond []                       p = p
cond ((predicate, branch):ps) p = do
    bVal <- predicate p
    if bVal then branch p
            else cond ps p

-- Performs `f` on the program ensuing changes to the variable or function
-- environment are not persistented outside the block. I.e. after finishing
-- executing the statement, the variable and function environments return to
-- how they were before the statement.
block :: (AppConfig -> AppConfig) -> AppConfig -> AppConfig
block f p = do
    oldConfig <- p
    newConfig <- f p
    return (resetEnv oldConfig newConfig)

-- Retrieves the value of a variable, throwing an undefined variable error if
-- the variable has not be defined.
getVarVal :: VarName -> AppConfig -> App TapeSymbol
getVarVal name p = do
    config <- p
    let val = lookupVar name config
    maybe (throwError (UndefVar name)) return val

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> AppConfig -> App TapeSymbol
derivedSymbolVal (Read)        = fmap getCurr
derivedSymbolVal (Var name)    = getVarVal name
derivedSymbolVal (Literal sym) = const (return sym)

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> AppConfig -> App Bool
bexpVal (TRUE)      _ = return True
bexpVal (FALSE)     _ = return False
bexpVal (Not b)     p = liftM not (bexpVal b p)
bexpVal (And b1 b2) p = liftM2 (&&) (bexpVal b1 p) (bexpVal b2 p)
bexpVal (Or b1 b2)  p = liftM2 (||) (bexpVal b1 p) (bexpVal b2 p)
bexpVal (Eq s1 s2)  p = liftM2 (==) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)
bexpVal (Le s1 s2)  p = liftM2 (<=) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)
bexpVal (Ne s1 s2)  p = liftM2 (/=) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: AppConfig -> AppConfig
evalLeft = fmap left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: AppConfig -> AppConfig
evalRight = fmap right

-- Evaluates writing to the tape.
evalWrite :: DerivedSymbol -> AppConfig -> AppConfig
evalWrite sym p = do
    val <- derivedSymbolVal sym p
    fmap (setCurr val) p

-- Evaluates an if-else statement.
evalIf :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> AppConfig -> AppConfig
evalIf bexp ifStm elseIfClauses elseStm = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b, block (evalStm stm))) allClauses
    allClauses = ((bexp, ifStm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
evalWhile :: Bexp -> Stm -> AppConfig -> AppConfig
evalWhile b body = fix f where
    f loop = cond [(bexpVal b, loop . (block (evalStm body)))]

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> AppConfig -> AppConfig
evalVarDecl name sym p = do
    val <- derivedSymbolVal sym p
    fmap (addVar name val) p

-- Evaluates a function declaration.
evalFuncDecl :: FuncName -> FuncDeclArgs -> Stm -> AppConfig -> AppConfig
evalFuncDecl name args body = fmap (addFunc name args body)

-- Checks that the number of arguments to a function is the same as the number
-- of arguments the function declaration specified.
checkNumArgs :: FuncName -> FuncDeclArgs -> FuncCallArgs -> AppConfig -> AppConfig
checkNumArgs name ds cs p | (length ds) == (length cs) = p
                          | otherwise                  = throwError err where
                              err = WrongNumArgs name ds cs

-- Evaluates the body of a function, after adding any arguments to the variable
-- environment. The variable and function environments are reset after executing
-- the body.
evalFuncBody :: FuncName -> FuncDeclArgs -> FuncCallArgs -> Stm -> AppConfig -> AppConfig
evalFuncBody name ds cs body = (block evalBody) . check where
    check    = checkNumArgs name ds cs
    evalBody = (evalStm body) . addedVarsP
    -- A `AppConfig` where the arguments have been added to the environment.
    addedVarsP p' = foldr (uncurry evalVarDecl) p' zippedArgs
    zippedArgs    = zip ds cs

-- Evaluates a function call.
evalCall :: FuncName -> FuncCallArgs -> AppConfig -> AppConfig
evalCall name args p = do
    config <- p
    let fMaybe = lookupFunc name config
    maybe err eval fMaybe where
        err                   = throwError (UndefFunc name)
        eval (argNames, body) = evalFuncBody name argNames args body p


-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> AppConfig -> AppConfig
evalComp stm1 stm2 = (evalStm stm2) . (evalStm stm1)

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: Stm -> AppConfig -> AppConfig
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

-- Evaluates a program by importing any files, then evaluating the statement.
evalProgram :: Program -> AppConfig -> AppConfig
evalProgram (Program imports stm) = evalStm stm
