module Semantics.Stm (evalStm) where

import Control.Exception
import Data.Maybe (maybeToList)
import Semantics.Bexp
import Semantics.DerivedSymbol
import Semantics.Helpers
import State.App
import State.Config
import State.Error
import State.MachineClass
import State.Output
import State.Tape as Tape
import Syntax.Tree

-- Attempt to modify the tape, and throw if the tape does not exist.
modify :: (Monad m) => (Tape -> Tape) -> VarName -> Config -> App m Config
modify f tapeName c = tryMaybe (modifyTape tapeName f c) (UndefTape tapeName)

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: (Monad m) => VarName -> Config -> App m Config
evalLeft = modify left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: (Monad m) => VarName -> Config -> App m Config
evalRight = modify right

-- Evaluates writing to the tape.
evalWrite :: (Monad m) => VarName -> DerivedSymbol -> Config -> App m Config
evalWrite tapeName sym c = do
    val <- derivedSymbolVal sym c
    modify (setSym val) tapeName c

-- Evalutes writing a string to the tape. This is the same as individually
-- writing many symbols to the tape, and moving right in between writes.
evalWriteStr :: (Monad m) => VarName -> [TapeSymbol] -> Config -> App m Config
evalWriteStr tapeName []       config = return config
evalWriteStr tapeName [s]      config = evalWrite tapeName (Literal s) config
evalWriteStr tapeName (s:rest) config = do
    c'  <- evalWrite tapeName (Literal s) config
    c'' <- evalRight tapeName c'
    evalWriteStr tapeName rest c''

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

-- Evaluates a variable declaration.
evalVarDecl :: (Monad m) => VarName -> DerivedSymbol -> Config -> App m Config
evalVarDecl name sym config = do
    val <- derivedSymbolVal sym config
    return (putSym name val config)

-- Evalutes a tape declaration.
evalTapeDecl :: (Monad m) => VarName -> String -> Config -> App m Config
evalTapeDecl name contents config = return (newTape name tape config) where
    tape = Tape.fromString contents

-- Evaluates a function declaration.
evalFuncDecl :: (Monad m) => FuncName -> FuncDeclArgs -> Stm -> Config -> App m Config
evalFuncDecl name args body config = return (putFunc name args body config)

-- Checks that the number of arguments to a function is the same as the number
-- of arguments the function declaration specified.
checkNumArgs :: (Monad m) => FuncName -> FuncDeclArgs -> FuncCallArgs -> Config -> App m Config
checkNumArgs name ds cs config | (length ds) == (length cs) = return config
                               | otherwise = throw err where
                                   err = WrongNumArgs name ds cs

-- Binds function arguments to values supplied to the function.
bindFuncArg :: (Monad m) => (FuncDeclArg, DerivedSymbol) -> App m Config -> App m Config
bindFuncArg ((FuncDeclArg name    SymType),  sym)          app = app >>= evalVarDecl name sym
bindFuncArg ((FuncDeclArg newName TapeType), Var tapeName) app = do
    config <- app
    tryMaybe (putTapeRef newName tapeName config) (UndefTape tapeName)
bindFuncArg _ app = error "types"

-- Evaluates the body of a function, after adding any arguments to the variable
-- environment. The variable and function environments are reset after executing
-- the body.
evalFuncBody :: (MonadOutput m) => FuncName -> FuncDeclArgs -> FuncCallArgs -> Stm -> Config -> App m Config
evalFuncBody name ds cs body config = do
    -- Check the number of arguments to the function is the correct.
    let app = checkNumArgs name ds cs config
    let zippedArgs = zip ds cs
    --let f (name', sym) app' = evalVarDecl name' sym =<< app'
    -- A config where the arguments have been added to the environment.
    addedVarsConfig <- foldr bindFuncArg app zippedArgs
    block (evalStm body) addedVarsConfig

-- Evaluates a function call.
evalCall :: (MonadOutput m) => FuncName -> FuncCallArgs -> Config -> App m Config
evalCall name args config = do
    let fMaybe = getFunc name config
    maybe err eval fMaybe where
        err                   = throw (UndefFunc name)
        eval (argNames, body) = evalFuncBody name argNames args body config

-- Evaluates the composition of two statements.
evalComp :: (MonadOutput m) => Stm -> Stm -> Config -> App m Config
evalComp stm1 stm2 config = (evalStm stm1 config) >>= (evalStm stm2)

-- Evaluates printing the current symbol.
evalPrintRead :: (MonadOutput m) => VarName -> Config -> App m Config
evalPrintRead tapeName c = do
    sym <- derivedSymbolVal (Read tapeName) c
    output' [sym] c

-- Evaluates printing a string.
evalPrintStr :: (MonadOutput m) => String -> Config -> App m Config
evalPrintStr = output'

-- Evalautes a statement in a configuration of a Turing machine.
evalStm :: (MonadOutput m) => Stm -> Config -> App m Config
evalStm (MoveLeft tapeName)       = evalLeft tapeName
evalStm (MoveRight tapeName)      = evalRight tapeName
evalStm (Write sym tapeName)      = evalWrite tapeName sym
evalStm (WriteStr str tapeName)   = evalWriteStr tapeName str
evalStm (Accept)                  = const accept
evalStm (Reject)                  = const reject
evalStm (If b stm elseIf elseStm) = evalIf b stm elseIf elseStm
evalStm (While b stm)             = evalWhile b stm
evalStm (VarDecl name sym)        = evalVarDecl name sym
evalStm (TapeDecl name contents)  = evalTapeDecl name contents
evalStm (FuncDecl name args body) = evalFuncDecl name args body
evalStm (Call name args)          = evalCall name args
evalStm (Comp stm1 stm2)          = evalComp stm1 stm2
evalStm (PrintRead tapeName)      = evalPrintRead tapeName
evalStm (PrintStr str)            = evalPrintStr str
