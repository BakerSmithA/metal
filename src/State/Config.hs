module State.Config where

import Data.Map as Map
import State.Tape as Tape
import Syntax.Tree

-- A configuration of a Turing machine.
data Config = Config {
    tapes :: Map TapeName Tape                -- The tape of the Turing machine.
  , vars  :: Map VarName TapeSymbol           -- The environment of variable declarations.
  , funcs :: Map FuncName (FuncDeclArgs, Stm) -- The environment of function declarations.
} deriving (Eq)

instance Show Config where
    -- show :: Config -> String
    show (Config ts vs fs) = "tapes: "   ++ (show ts)
                          ++ ", vars: "  ++ (show vs)
                          ++ ", funcs: " ++ (show (keys fs))

-- Config which contains no tapes.
empty :: Config
empty = Config Map.empty Map.empty Map.empty

-- A configuration in which the read-write head is in the zeroed position, and
-- `str` is at the start of the tape.
fromString :: TapeName -> String -> Config
fromString tapeName str = State.Config.empty { tapes = ts } where
    ts = singleton tapeName (Tape.fromString str)

-- Looks up a tape in an environment.
getTape :: TapeName -> Config -> Maybe Tape
getTape name (Config ts _ _) = Map.lookup name ts

-- Sets a tape in the environment.
putTape :: TapeName -> Tape -> Config -> Config
putTape name tape c = c { tapes = Map.insert name tape (tapes c) }

-- Retrieves and then modifies a tape in the environment.
modifyTape :: TapeName -> (Tape -> Tape) -> Config -> Maybe Config
modifyTape name f config = do
    tape <- getTape name config
    return $ putTape name (f tape) config

-- Looks up a variable in an environment.
getVar :: VarName -> Config -> Maybe TapeSymbol
getVar name config = Map.lookup name (vars config)

-- Looks up a function in an environment.
getFunc :: FuncName -> Config -> Maybe (FuncDeclArgs, Stm)
getFunc name config = Map.lookup name (funcs config)

-- Adds a single variable to the environment.
putVar :: VarName -> TapeSymbol -> Config -> Config
putVar name sym config = config { vars = Map.insert name sym (vars config) }

-- Adds a single function to the environment.
putFunc :: FuncName -> FuncDeclArgs -> Stm -> Config -> Config
putFunc name args body config = config { funcs = Map.insert name (args, body) (funcs config) }

-- Resets the variable and function environment of `cNew` to that provided
-- by `cOld`.
resetEnv :: Config -> Config -> Config
resetEnv cOld cNew = cNew { tapes = tapes cOld, vars = vars cOld, funcs = funcs cOld }
