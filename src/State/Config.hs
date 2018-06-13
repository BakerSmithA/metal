module State.Config where

import Data.Map as Map
import State.Tape as Tape
import Syntax.Tree

type Address = Integer
type Object = Map VarName Variable

data Variable = Symbol TapeSymbol
              | Ptr Address
              deriving (Eq, Show)

data Reference = TapeRef Tape
               | ObjRef Object
               deriving (Eq, Show)

-- A configuration of a Turing machine.
data Config = Config {
    vars      :: Map VarName Variable
  , funcs     :: Map FuncName ([FuncDeclArg], Stm)
  , refs      :: Map Address Reference
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
fromString tapeName str = newRef tapeName tape State.Config.empty where
    tape = TapeRef (Tape.fromString str)

-- Retrieves a tape or symbol from the variable environment.
getVar :: VarName -> Config -> Maybe Variable
getVar name c = Map.lookup name (vars c)

-- Puts a variable in the environment, overwriting whatever variable is
-- currently in the environment with the same name.
putVar :: VarName -> Variable -> Config -> Config
putVar name var c = c { vars = Map.insert name var (vars c) }

--------------------------------------------------------------------------------
-- Tape Symbols
--------------------------------------------------------------------------------

-- Looks up a variable in an environment.
getSym :: VarName -> Config -> Maybe TapeSymbol
getSym name c = do
    (Symbol sym) <- getVar name c
    return sym

-- Adds a variable to the environment.
putSym :: VarName -> TapeSymbol -> Config -> Config
putSym name sym = putVar name (Symbol sym)

--------------------------------------------------------------------------------
-- Tapes
--------------------------------------------------------------------------------

-- Looks up the address of a object in the environment.
getPtr :: VarName -> Config -> Maybe Address
getPtr name c = do
    (Ptr addr) <- getVar name c
    return addr

-- Deferences a pointer to a object.
derefPtr :: Address -> Config -> Maybe Reference
derefPtr addr c = Map.lookup addr (refs c)

-- Adds a pointer to a object to the environment.
putPtr :: VarName -> Address -> Config -> Config
putPtr name addr = putVar name (Ptr addr)

-- Adds an object to the environment, returning the address at which the object
-- was added.
putRef :: Reference -> Config -> (Address, Config)
putRef _     (Config _ _ _ [])              = error "No space for tapes left"
putRef ref c@(Config _ _ _ (freeAddr:rest)) = (freeAddr, c') where
    c'    = c { refs = refs', freeAddrs = rest }
    refs' = Map.insert freeAddr ref (refs c)

-- Creates a pointer to a new object with the given name and contents.
newRef :: VarName -> Reference -> Config -> Config
newRef name ref c = putPtr name addr c' where
    (addr, c') = putRef ref c

-- Modifies the object at the address, if it exists.
modifyRef :: Address -> (Reference -> Maybe Reference) -> Config -> Maybe Config
modifyRef addr f c = do
    ref <- derefPtr addr c
    let ref' = f ref
    let refs' = maybe (refs c) (\r -> Map.insert addr r (refs c)) ref'
    return c { refs = refs' }

-- Tries to modify the tape at the address. If there is no tape specifically at
-- the address then returns Nothing.
modifyTape :: Address -> (Tape -> Tape) -> Config -> Maybe Config
modifyTape addr f = modifyRef addr modify where
    modify (TapeRef t) = Just (TapeRef (f t))
    modify _           = Nothing

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
