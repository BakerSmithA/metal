module Semantics.Program where

import Syntax.Tree
import State.App
import State.Config
import Semantics.Stm
import State.Output

-- Describes a path in the tree, this could represent a file system path.
type TreePath = String
type Tree m = (TreePath -> m ([TreePath], Stm))

-- Perfoms a DF search resolving imports into the statement in those files.
-- `tree` describes the shape of the tree by giving a list of branches
-- (import statements) in that file.
importStms :: (Monad m) => Tree m -> [TreePath] -> m [Stm]
importStms _ [] = return []
importStms tree (path:rest) = do
    (imports, stm) <- tree path
    childrenStms <- importStms tree imports
    restStms <- importStms tree rest
    return (childrenStms ++ [stm] ++ restStms)

-- Evalutes the program, and defaults to accepting if no terminating state is
-- reached.
evalProg :: (MonadOutput m) => Program -> Config -> App m Config
evalProg (Program _ body) = evalStm (Comp body Accept)
