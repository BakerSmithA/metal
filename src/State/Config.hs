module State.Config where

import Data.Map as Map
import State.Tape as Tape
import Syntax.Tree

-- A configuration of a Turing machine.
data Config = Config {
    pos   :: Pos                    -- The position the read-write head.
  , tape  :: Tape                   -- The tape of the Turing machine.
  , vars  :: Map VarName TapeSymbol -- The environment of variable declarations.
  , funcs :: Map FuncName Stm       -- The environment of function declarations.
} deriving (Eq)

instance Show Config where
    -- show :: Config -> String
    show (Config p t vs fs) = "pos: "     ++ (show p)
                           ++ ", tape: "  ++ (show t)
                           ++ ", vars: "  ++ (show vs)
                           ++ ", funcs: " ++ (show fs)

-- A configuration in which the read-write head is in the zeroed position, and
-- `str` is at the start of the tape.
fromString :: String -> Config
fromString str = Config 0 (Tape.fromString str) Map.empty Map.empty

-- A configuration in which the read-writ head is in the zeroed position, and
-- the tape is empty.
initial :: Config
initial = State.Config.fromString ""

-- Moves the read-write head one cell to the left, provided the head is not in
-- the zeroed position, in this case no action occurs.
left :: Config -> Config
left c = c { pos = max p' 0 } where
    p' = pos c - 1

-- Moves the read-write head one cell to the right.
right :: Config -> Config
right c = c { pos = (pos c) + 1 }

-- Reads the symbol under the read-write head.
getCurr :: Config -> TapeSymbol
getCurr (Config p t vs fs) = getSym p t

-- Writes a symbol at the current position of the read-write head.
setCurr :: TapeSymbol -> Config -> Config
setCurr sym (Config p t vs fs) = Config p t' vs fs where
    t' = setSym p sym t

-- Looks up a variable in an environment.
lookupVar :: VarName -> Config -> Maybe TapeSymbol
lookupVar name config = Map.lookup name (vars config)

-- Looks up a function in an environment.
lookupFunc :: FuncName -> Config -> Maybe Stm
lookupFunc name config = Map.lookup name (funcs config)

-- Adds a single variable to the environment.
addVar :: VarName -> TapeSymbol -> Config -> Config
addVar name sym config = config { vars = insert name sym (vars config) }

-- Adds a single function to the environment.
addFunc :: FuncName -> Stm -> Config -> Config
addFunc name body config = config { funcs = insert name body (funcs config) }

-- Resets the variable and function environment of `cNew` to that provided
-- by `cOld`.
resetEnv :: Config -> Config -> Config
resetEnv cOld cNew = cNew { vars = vars cOld, funcs = funcs cOld }
