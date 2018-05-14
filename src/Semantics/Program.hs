module Semantics.Program where

import Syntax.Tree
import Syntax.Parser
import State.App
import State.Config
import Semantics.Stm
import State.Output

-- Describes a path in the tree, this could represent a file system path.
type Tree m = (ImportPath -> m ([ImportPath], Stm))

-- Perfoms a DF search resolving imports into the statement in those files.
-- `tree` describes the shape of the tree by giving a list of branches
-- (import statements) in that file.
importStms :: (Monad m) => Tree m -> [ImportPath] -> m [Stm]
importStms _ [] = return []
importStms tree (path:rest) = do
    (imports, stm) <- tree path
    childrenStms <- importStms tree imports
    restStms <- importStms tree rest
    return (childrenStms ++ [stm] ++ restStms)

-- Uses the file system to read a Metal file and parse the input.
ioTree :: ImportPath -> IO ([ImportPath], Stm)
ioTree path = undefined

-- Evalutes the program, and defaults to accepting if no terminating state is
-- reached.
evalProg :: (MonadOutput m) => Program -> Config -> App m Config
evalProg (Program _ body) = evalStm (Comp body Accept)
