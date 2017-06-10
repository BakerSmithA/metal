module Semantics.Denotational where

import Syntax.Tree
import State.MachineState
import State.Config
import State.Env
import State.Error
import Control.Monad.Except
import Control.Monad.Reader

type State = ExceptT RuntimeError IO Machine

-- Evaluates moving the read-write head one cell to the left.
evalLeft :: Config -> Machine
evalLeft c = Inter (left c)

-- Evaluates calling a function.
evalCall :: FuncName -> Config -> Either RuntimeError Machine
evalCall = undefined

-- Evaluates printing a string.
evalPrintStr :: String -> Config -> IO Machine
evalPrintStr str c = do
    putStrLn str
    return (Inter c)

-- Evalautes a statement in a configuration of a Turing machine.
evalS :: Stm -> Config -> State
evalS (MoveLeft)   c = liftIO (return (evalLeft c))
evalS (PrintStr s) c = liftIO (evalPrintStr s c)
evalS (Call f)     c = throwError UndefFunc

-- Prints the result of a computation, either that an error occurred, or the
-- state the machine halted in.
printResult :: Either RuntimeError Machine -> IO ()
printResult res = putStrLn $ case res of
    Right mach      -> show mach
    Left  UndefVar  -> "Undefined variable"
    Left  UndefFunc -> "Undefined function"
