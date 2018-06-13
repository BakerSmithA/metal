module Semantics.Stm (evalStm) where

import Control.Exception
import Data.Maybe (maybeToList)
import Semantics.Bexp
import Semantics.Variable
import Semantics.Helpers
import State.App
import State.Config hiding (modifyTape)
import qualified State.Config as Config (modifyTape)
import State.Error
import State.MachineClass
import State.Output
import State.Tape as Tape
import Syntax.Tree

-- Attempt to modify the tape, and throw if the tape does not exist.
modifyTape :: (Monad m) => (Tape -> Tape) -> TapeExpr -> Config -> App m Config
modifyTape f tapeExpr c = do
    (addr, c') <- tapePtr tapeExpr c
    tryMaybe (Config.modifyTape addr f c') UndefVar

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: (Monad m) => TapeExpr -> Config -> App m Config
evalLeft = modifyTape left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: (Monad m) => TapeExpr -> Config -> App m Config
evalRight = modifyTape right

-- Evaluates writing to the tape.
evalWrite :: (Monad m) => TapeExpr -> SymExpr -> Config -> App m Config
evalWrite tapeExpr symExpr c = do
    (val, c') <- symVal symExpr c
    modifyTape (setSym val) tapeExpr c'

-- Evaluates an if-else statement.
evalIf :: (MonadOutput m) => Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> Config -> App m Config
evalIf bexp ifStm elseIfClauses elseStm = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b, block (evalStm stm))) allClauses
    allClauses = ((bexp, ifStm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
evalWhile :: (MonadOutput m) => Bexp -> Stm -> Config -> App m Config
evalWhile b body = fix f where
    f loop = cond [(bexpVal b, evalLoop)] where
        evalLoop c = block (evalStm body) c >>= loop

-- Evaluates a symbol declaration.
evalSymDecl :: (Monad m) => VarName -> SymExpr -> Config -> App m Config
evalSymDecl name symExpr c = do
    (val, c') <- symVal symExpr c
    return (putSym name val c')

-- Evaluates a tape declaration.
evalTapeDecl :: (Monad m) => VarName -> TapeExpr -> Config -> App m Config
evalTapeDecl name tapeExpr c = do
    (addr, c') <- tapePtr tapeExpr c
    return (putTapePtr name addr c')

-- Evaluates a variable declaration.
evalVarDecl :: (Monad m) => VarName -> AnyValExpr -> Config -> App m Config
evalVarDecl name (S s) = evalSymDecl name s
evalVarDecl name (T t) = evalTapeDecl name t

-- Evaluates a function declaration.
evalFuncDecl :: (Monad m) => FuncName -> [FuncDeclArg] -> Stm -> Config -> App m Config
evalFuncDecl name args body config = return (putFunc name args body config)

-- Checks that the number of arguments to a function is the same as the number
-- of arguments the function declaration specified.
checkNumArgs :: (Monad m) => FuncName -> [FuncDeclArg] -> [FuncCallArg] -> Config -> App m Config
checkNumArgs name ds cs config | (length ds) == (length cs) = return config
                               | otherwise = throw err where
                                   err = WrongNumArgs name ds cs

-- Binds function arguments to values supplied to the function.
bindFuncArg :: (Monad m) => (FuncDeclArg, FuncCallArg) -> App m Config -> App m Config
bindFuncArg ((name, _), valExpr) app = app >>= evalVarDecl name valExpr

-- Evaluates the body of a function, after adding any arguments to the variable
-- environment. The variable and function environments are reset after executing
-- the body.
evalFuncBody :: (MonadOutput m) => FuncName -> [FuncDeclArg] -> [FuncCallArg] -> Stm -> Config -> App m Config
evalFuncBody name ds cs body config = do
    -- Check the number of arguments to the function is the correct.
    let app = checkNumArgs name ds cs config
    let zippedArgs = zip ds cs
    -- A config where the arguments have been added to the environment.
    addedVarsConfig <- foldr bindFuncArg app zippedArgs
    newConfig <- block (evalStm body) addedVarsConfig
    oldConfig <- app
    -- Reset the environment so variables declared as function arguments do not
    -- 'leak' out.
    return (revertEnv oldConfig newConfig)

-- Evaluates a function call.
evalCall :: (MonadOutput m) => FuncName -> [FuncCallArg] -> Config -> App m Config
evalCall name args config = do
    let fMaybe = getFunc name config
    maybe err eval fMaybe where
        err                   = throw (UndefFunc name)
        eval (argNames, body) = evalFuncBody name argNames args body config

-- Evaluates the composition of two statements.
evalComp :: (MonadOutput m) => Stm -> Stm -> Config -> App m Config
evalComp stm1 stm2 config = (evalStm stm1 config) >>= (evalStm stm2)

-- Evaluates printing the current symbol.
evalPrintRead :: (MonadOutput m) => SymExpr -> Config -> App m Config
evalPrintRead symExpr c = do
    (sym, c') <- symVal symExpr c
    output' [sym] c'

-- Evalutes debug printing the contents of a tape.
evalDebugPrintTape :: (MonadOutput m) => TapeExpr -> Config -> App m Config
evalDebugPrintTape tapeExpr c1 = do
    (addr, c2) <- tapePtr tapeExpr c1
    tape <- tryMaybe (derefTape addr c2) UndefVar
    output' (show tape) c2

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: (MonadOutput m) => Stm -> Config -> App m Config
evalStm (MoveLeft tapeExpr)       = evalLeft tapeExpr
evalStm (MoveRight tapeExpr)      = evalRight tapeExpr
evalStm (Write tapeExpr sym)      = evalWrite tapeExpr sym
evalStm (Accept)                  = const accept
evalStm (Reject)                  = const reject
evalStm (If b stm elseIf elseStm) = evalIf b stm elseIf elseStm
evalStm (While b stm)             = evalWhile b stm
evalStm (VarDecl name expr)       = evalVarDecl name expr
evalStm (FuncDecl name args body) = evalFuncDecl name args body
evalStm (Call name args)          = evalCall name args
evalStm (Comp stm1 stm2)          = evalComp stm1 stm2
evalStm (Print symExpr)           = evalPrintRead symExpr
evalStm (DebugPrintTape tapeExpr) = evalDebugPrintTape tapeExpr
