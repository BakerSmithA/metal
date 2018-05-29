module State.Config where

import Data.Map as Map
import State.Tape as Tape
import Syntax.Tree

-- A configuration of a Turing machine.
data Config = Config {
    tapes :: Map TapeName (Tape, Pos)         -- The tape of the Turing machine.
  , vars  :: Map VarName TapeSymbol           -- The environment of variable declarations.
  , funcs :: Map FuncName (FuncDeclArgs, Stm) -- The environment of function declarations.
} deriving (Eq)

instance Show Config where
    -- show :: Config -> String
    show (Config ts vs fs) = "tapes: "  ++ (show ts)
                          ++ ", vars: "  ++ (show vs)
                          ++ ", funcs: " ++ (show (keys fs))

-- A configuration in which the read-write head is in the zeroed position, and
-- `str` is at the start of the tape.
fromString :: TapeName -> String -> Config
fromString tapeName str = Config ts Map.empty Map.empty where
    ts = singleton tapeName (Tape.fromString str, 0)

-- A configuration in which the read-writ head is in the zeroed position, and
-- the tape is empty.
initial :: TapeName -> Config
initial tapeName = State.Config.fromString tapeName ""

-- Checks the required tape exists and the performs the operation. If it does
-- not exist then Nothing is returned.
adjustTape :: ((Tape, Pos) -> (Tape, Pos)) -> TapeName -> Config -> Maybe Config
adjustTape op tapeName c@(Config ts _ _) = do
    t <- Map.lookup tapeName ts
    return (c { tapes = Map.insert tapeName (op t) ts })

-- Moves the read-write head, provided the head is not moved left past the zero
-- position, in this case no action occurs.
-- Returns Nothing if the tape does not exist.
move :: Integer -> TapeName -> Config -> Maybe Config
move dist = adjustTape $ \(tape, pos) -> (tape, max 0 (pos+dist))

-- Moves the read-write head one cell to the left, provided the head is not in
-- the zeroed position, in this case no action occurs.
left :: TapeName -> Config -> Maybe Config
left = move (-1)

-- Moves the read-write head one cell to the right.
right :: TapeName -> Config -> Maybe Config
right = move 1

-- Reads the symbol under the read-write head.
getCurr :: TapeName -> Config -> Maybe TapeSymbol
getCurr tapeName (Config ts _ _) = do
    (tape, pos) <- Map.lookup tapeName ts
    return (getSym pos tape)

-- Writes a symbol at the current position of the read-write head.
setCurr :: TapeName -> TapeSymbol -> Config -> Maybe Config
setCurr tapeName sym = adjustTape (\(tape, pos) -> (setSym pos sym tape, pos)) tapeName

-- Looks up a variable in an environment.
lookupVar :: VarName -> Config -> Maybe TapeSymbol
lookupVar name config = Map.lookup name (vars config)

-- Looks up a function in an environment.
lookupFunc :: FuncName -> Config -> Maybe (FuncDeclArgs, Stm)
lookupFunc name config = Map.lookup name (funcs config)

-- Adds a single variable to the environment.
addVar :: VarName -> TapeSymbol -> Config -> Config
addVar name sym config = config { vars = Map.insert name sym (vars config) }

-- Adds a single function to the environment.
addFunc :: FuncName -> FuncDeclArgs -> Stm -> Config -> Config
addFunc name args body config = config { funcs = Map.insert name (args, body) (funcs config) }

-- Resets the variable and function environment of `cNew` to that provided
-- by `cOld`.
resetEnv :: Config -> Config -> Config
resetEnv cOld cNew = cNew { vars = vars cOld, funcs = funcs cOld }
