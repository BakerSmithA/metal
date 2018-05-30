module State.Config where

import Data.Map as Map
import State.Tape as Tape
import Syntax.Tree

type Address = Integer

data Variable = Symbol TapeSymbol
              | TapeRef Address
              deriving (Show, Eq)

-- A configuration of a Turing machine.
data Config = Config {
    vars      :: Map VarName Variable
  , funcs     :: Map FuncName (FuncDeclArgs, Stm)
  , refs      :: Map Address Tape
  , freeAddrs :: [Address]
} deriving (Eq)

instance Show Config where
    -- show :: Config -> String
    show (Config vs fs rs _) = "vars: " ++ (show vs)
                            ++ "refs: " ++ (show rs)
                            ++ ", funcs: " ++ (show (keys fs))

-- Config which contains nothing.
empty :: Config
empty = Config Map.empty Map.empty Map.empty [0..1000] -- HACK: Infinite list causes hanging.

-- A configuration in which the read-write head is in the zeroed position, and
-- `str` is at the start of the tape.
fromString :: VarName -> String -> Config
fromString tapeName str = newTape tapeName (Tape.fromString str) State.Config.empty

-- fromString tapeName str = State.Config.empty { vars = ts } where
--     ts = singleton tapeName (TapeRef $ Tape.fromString str)

-- Retrieves a tape or symbol from the variable environment.
getVariable :: VarName -> Config -> Maybe Variable
getVariable name c = Map.lookup name (vars c)

-- Looks up the address of a tape in the environment.
getTapeRef :: VarName -> Config -> Maybe Address
getTapeRef name config = do
    (TapeRef addr) <- getVariable name config
    return addr

-- Looks up a tape and its address in an environment.
getTapeData :: VarName -> Config -> Maybe (Address, Tape)
getTapeData name config = do
    addr <- getTapeRef name config
    tape <- Map.lookup addr (refs config)
    return (addr, tape)

-- Looks up a tape in an environment.
getTape :: VarName -> Config -> Maybe Tape
getTape name c = fmap snd (getTapeData name c)

-- Creates a new tape in the environment.
newTape :: VarName -> Tape -> Config -> Config
newTape _ _ (Config _ _ _ []) = error "No space for tapes left"
newTape name tape c@(Config vs _ rs (freeAddr:rest)) = c { vars = vs', refs = rs', freeAddrs = rest } where
    vs' = Map.insert name (TapeRef freeAddr) vs
    rs' = Map.insert freeAddr tape rs

-- Sets a reference to an existing tape with the given name.
putTapeRef :: VarName -> VarName -> Config -> Maybe Config
putTapeRef newName existingName c = do
    addr <- getTapeRef existingName c
    return $ c { vars = Map.insert newName (TapeRef addr) (vars c) }

-- Adds a tape at the given address.
putTape :: Address -> Tape -> Config -> Config
putTape addr tape c = c { refs = Map.insert addr tape (refs c) }

-- Retrieves and then modifies a tape in the environment.
modifyTape :: VarName -> (Tape -> Tape) -> Config -> Maybe Config
modifyTape name f config = do
    (addr, tape) <- getTapeData name config
    return $ putTape addr (f tape) config

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
