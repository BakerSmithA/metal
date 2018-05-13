module Semantics.Program where

import Syntax.Tree
import State.App
import State.Config
import Semantics.Stm
import State.Output

-- Describes a path in the tree, this could represent a file system path.
type TreePath = String

-- Perfoms a depth first search resolving imports into the statement in those
-- files. `tree` describes the shape of the tree by giving a list of branches
-- (import statements) in that file.
importStms :: (TreePath -> ([TreePath], Stm)) -> TreePath -> [Stm]
importStms tree initialPath = foldr f [body] imports where
    f path acc = (importStms tree path) ++ acc
    (imports, body) = tree initialPath

-- Evalutes the program, and defaults to accepting if no terminating state is
-- reached.
evalProg :: (MonadOutput m) => Program -> Config -> App m Config
evalProg (Program _ body) = evalStm (Comp body Accept)
