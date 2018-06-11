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
  , funcs     :: Map FuncName ([FuncDeclArg], Stm)
  , refs      :: Map Address Tape
  , freeAddrs :: [Address]
} deriving (Eq)

instance Show Config where
    -- show :: Config -> String
    show (Config vs fs rs _) =   "vars: " ++ (show vs)
                            ++ ", refs: " ++ (show rs)
                            ++ ", funcs: " ++ (show (keys fs))

-- Config which contains nothing.
empty :: Config
empty = Config Map.empty Map.empty Map.empty [0..1000] -- HACK: Infinite list causes hanging.

-- A configuration in which the read-write head is in the zeroed position, and
-- `str` is at the start of the tape.
fromString :: VarName -> String -> Config
fromString tapeName str = newTape tapeName (Tape.fromString str) State.Config.empty

-- Retrieves a tape or symbol from the variable environment.
getVariable :: VarName -> Config -> Maybe Variable
getVariable name c = Map.lookup name (vars c)

--------------------------------------------------------------------------------
-- Tape Symbols
--------------------------------------------------------------------------------

-- Looks up a variable in an environment.
getSym :: VarName -> Config -> Maybe TapeSymbol
getSym name c = do
    (Symbol sym) <- getVariable name c
    return sym

-- Adds a variable to the environment.
putSym :: VarName -> TapeSymbol -> Config -> Config
putSym name sym c = c { vars = Map.insert name (Symbol sym) (vars c) }

--------------------------------------------------------------------------------
-- Tapes
--------------------------------------------------------------------------------

-- Looks up the address of a tape in the environment.
getTapePtr :: VarName -> Config -> Maybe Address
getTapePtr name c = do
    (TapeRef addr) <- getVariable name c
    return addr

-- Deferences a pointer to a tape.
derefTape :: Address -> Config -> Maybe Tape
derefTape addr c = Map.lookup addr (refs c)

-- Looks up the address of the tape and then deferences it, therefore changes
-- to the returned tape will not be reflected in the environment.
getTapeCpy :: VarName -> Config -> Maybe Tape
getTapeCpy name c = getTapePtr name c >>= \addr -> derefTape addr c

-- Adds a pointer to a tape to the environment.
putTapePtr :: VarName -> Address -> Config -> Config
putTapePtr name addr c = c { vars = Map.insert name (TapeRef addr) (vars c) }

-- Adds a tape to the environment, returning the address at which the tape was
-- added.
putTape :: Tape -> Config -> (Address, Config)
putTape _      (Config _ _ _ [])              = error "No space for tapes left"
putTape tape c@(Config _ _ _ (freeAddr:rest)) = (freeAddr, c') where
    c'    = c { refs = refs', freeAddrs = rest }
    refs' = Map.insert freeAddr tape (refs c)

-- Creates a pointer to a new tape with the given name and contents.
newTape :: VarName -> Tape -> Config -> Config
newTape name tape c = putTapePtr name addr c' where
    (addr, c') = putTape tape c

-- Modifies the tape at the address, if it exists.
modifyTape :: Address -> (Tape -> Tape) -> Config -> Maybe Config
modifyTape addr f c = do
    tape <- derefTape addr c
    let tape' = f tape
    return c { refs = Map.insert addr tape' (refs c) }

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- Looks up a function in an environment.
getFunc :: FuncName -> Config -> Maybe ([FuncDeclArg], Stm)
getFunc name c = Map.lookup name (funcs c)

-- Adds a function to the environment.
putFunc :: FuncName -> [FuncDeclArg] -> Stm -> Config -> Config
putFunc name args body c = c { funcs = Map.insert name (args, body) (funcs c) }

-- Resets the variable and function environment of `cNew` to that provided
-- by `cOld`. Also frees any references that are in the new environment but not
-- in the old.
revertEnv :: Config -> Config -> Config
revertEnv cOld cNew = Config (vars cOld) (funcs cOld) refs' freeAddrs' where
    refs' = Map.intersection (refs cNew) (refs cOld)
    freeAddrs' = (freeAddrs cOld) ++ removedAddrs
    removedAddrs = Map.keys (Map.difference (refs cNew) (refs cOld))
