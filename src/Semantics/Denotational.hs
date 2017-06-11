module Semantics.Denotational where

import Syntax.Tree
import State.MachineState
import State.Config
import State.Env
import State.Error
import Data.Maybe

--type State = Reader Env Machine
type State = Machine

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> Env -> Config -> TapeSymbol
derivedSymbolVal (Read)        env c = getCurr c
derivedSymbolVal (Literal sym) env c = sym
derivedSymbolVal (Var name)    env c = case lookupVar name env of
    Just sym -> sym
    Nothing  -> error ("No variable named: " ++ name)

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> Env -> Config -> Bool
bexpVal (TRUE)      env c = True
bexpVal (FALSE)     env c = False
bexpVal (Not b)     env c = not (bexpVal b env c)
bexpVal (And b1 b2) env c = (bexpVal b1 env c) && (bexpVal b2 env c)
bexpVal (Or b1 b2)  env c = (bexpVal b1 env c) || (bexpVal b2 env c)
bexpVal (Eq s1 s2)  env c = (derivedSymbolVal s1 env c) == (derivedSymbolVal s2 env c)
bexpVal (Le s1 s2)  env c = (derivedSymbolVal s1 env c) <= (derivedSymbolVal s2 env c)

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(Config -> Bool, Config -> State)] -> (Config -> State)
cond []               c = undefined
cond ((p, branch):bs) c = if p c then branch c else cond bs c

-- Fixpoint operator used to defined loops.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: Config -> State
evalLeft = return . left

-- Evaluates moving the read-write head one cell to the right.
evalRight :: Config -> State
evalRight = return . right

-- Evaluates writing to the tape.
evalWrite :: TapeSymbol -> Config -> State
evalWrite derSym = return . (setCurr derSym)

-- Evaluates halting the machine by accepting.
evalAccept :: State
evalAccept = HaltA

-- Evaluates halting the machine by rejecting.
evalReject :: State
evalReject = HaltR

-- Evaluates an if-else statement.
evalIfElse :: Bexp -> Stm -> [(Bexp, Stm)] -> Maybe Stm -> Env -> Config -> State
evalIfElse b stm elseIfClauses elseStm env = cond branches where
    branches   = map (\(b, stm) -> (bexpVal b env, evalS stm env)) allClauses
    allClauses = ((b, stm):elseIfClauses) ++ (maybeToList elseClause)
    elseClause = fmap (\stm -> (TRUE, stm)) elseStm

-- Evaluates a while loop.
evalWhile :: Bexp -> Stm -> Env -> Config -> State
evalWhile b stm env = fix f where
    f g = cond [(bexpVal b env, \c -> evalS stm env c >>= g)]

-- Evaluates a variable declaration.
evalVarDecl :: VarName -> DerivedSymbol -> Env -> Config -> State
evalVarDecl name derSym env config = undefined

-- evalDeclFunc :: FuncName -> Stm -> Config -> State
-- evalDeclFunc fName stm (tape, pos, envv, envf) = Inter (tape, pos, envv, envf') where
--     envf' = update fName (Just stm) envf

-- Evaluates a function declaration.
evalFuncDecl :: FuncName -> Stm -> Env -> Config -> State
evalFuncDecl name body config = undefined

-- Evaluates a function call.
evalCall :: FuncName -> Env -> Config -> State
evalCall = undefined

-- evalCall name config = do
--     -- TODO: Error handling.
--     Just body <- asks (lookupFunc name)
--     evalS body config

-- Evaluates the composition of two statements.
evalComp :: Stm -> Stm -> Env -> Config -> State
evalComp = undefined

-- Evaluates print the symbol under the read-write head.
evalPrintRead :: Config -> State
evalPrintRead = undefined

-- Evaluates string an arbitrary string.
evalPrintStr :: String -> Config -> State
evalPrintStr = undefined

-- Evalautes a statement in a configuration of a Turing machine.
evalS :: Stm -> Env -> Config -> State
evalS (MoveLeft)            env = evalLeft
evalS (MoveRight)           env = evalRight
evalS (Write derSym)        env = \c -> evalWrite (derivedSymbolVal derSym env c) c
evalS (Reject)              env = \c -> evalReject
evalS (Accept)              env = \c -> evalAccept
evalS (If b stm elseif els) env = evalIfElse b stm elseif els env
evalS (While b stm)         env = evalWhile b stm env
evalS (VarDecl name derSym) env = evalVarDecl name derSym env
evalS (FuncDecl name body)  env = evalFuncDecl name body env
evalS (Call name)           env = evalCall name env
evalS (PrintRead)           env = evalPrintRead
evalS (PrintStr str)        env = evalPrintStr str

-- Prints the result of the computation.
printResult :: Machine -> IO ()
printResult mach = putStrLn (show mach)
