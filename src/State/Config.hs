module State.Config where

import Data.Map as Map
import State.Tape as Tape
import Syntax.Tree

data Variable = Symbol TapeSymbol
              | TapeRef Tape
              deriving (Show, Eq)

-- A configuration of a Turing machine.
data Config = Config {
    vars  :: Map VarName Variable
  , funcs :: Map FuncName (FuncDeclArgs, Stm)
} deriving (Eq)

instance Show Config where
    -- show :: Config -> String
    show (Config vs fs) = "vars: "  ++ (show vs)
                       ++ ", funcs: " ++ (show (keys fs))

-- Config which contains nothing.
empty :: Config
empty = Config Map.empty Map.empty

-- A configuration in which the read-write head is in the zeroed position, and
-- `str` is at the start of the tape.
fromString :: VarName -> String -> Config
fromString tapeName str = State.Config.empty { vars = ts } where
    ts = singleton tapeName (TapeRef $ Tape.fromString str)

-- Retrieves a tape or symbol from the variable environment.
getVariable :: VarName -> Config -> Maybe Variable
getVariable name (Config vs _) = Map.lookup name vs

-- Looks up a tape in an environment.
getTape :: VarName -> Config -> Maybe Tape
getTape name config = do
    (TapeRef tape) <- getVariable name config
    return tape

-- Sets a tape in the environment.
putTape :: VarName -> Tape -> Config -> Config
putTape name tape c = c { vars = Map.insert name (TapeRef tape) (vars c) }

-- Retrieves and then modifies a tape in the environment.
modifyTape :: VarName -> (Tape -> Tape) -> Config -> Maybe Config
modifyTape name f config = do
    tape <- getTape name config
    return $ putTape name (f tape) config

-- Looks up a variable in an environment.
getSym :: VarName -> Config -> Maybe TapeSymbol
getSym name config = do
    (Symbol sym) <- getVariable name config
    return sym

-- Looks up a function in an environment.
getFunc :: FuncName -> Config -> Maybe (FuncDeclArgs, Stm)
getFunc name config = Map.lookup name (funcs config)

-- Adds a single variable to the environment.
putSym :: VarName -> TapeSymbol -> Config -> Config
putSym name sym config = config { vars = Map.insert name (Symbol sym) (vars config) }

-- Adds a single function to the environment.
putFunc :: FuncName -> FuncDeclArgs -> Stm -> Config -> Config
putFunc name args body config = config { funcs = Map.insert name (args, body) (funcs config) }

-- Resets the variable and function environment of `cNew` to that provided
-- by `cOld`.
resetEnv :: Config -> Config -> Config
resetEnv cOld cNew = cNew { vars = vars cOld, funcs = funcs cOld }
