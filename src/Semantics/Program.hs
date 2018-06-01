module Semantics.Program where

import Control.Exception
import Syntax.Tree
import Syntax.Parser
import State.App
import State.Config
import State.Output
import Semantics.Stm
import qualified Text.Megaparsec as M
import System.FilePath

-- Parses the contents of source file, returning either the parsed program, or
-- the parse error.
parseContents :: String -> IO Program
parseContents contents = do
    let result = parseM program "" contents
    either throw return result

-- Describes a path in the tree, this could represent a file system path.
type Tree m = (ImportPath -> m ([ImportPath], Stm))

-- Perfoms a DF search resolving imports into the statement in those files.
-- `tree` describes the shape of the tree by giving a list of branches
-- (import statements) in that file.
importStms :: (Monad m) => Tree m -> [ImportPath] -> m [Stm]
importStms _ [] = return []
importStms tree (path:rest) = do
    (imports, body) <- tree path
    childrenStms <- importStms tree imports
    restStms <- importStms tree rest
    return (childrenStms ++ [body] ++ restStms)

-- Uses the file system to read a Metal file and parse the input.
ioTree :: String -> ImportPath -> IO ([ImportPath], Stm)
ioTree dirPath path = do
    -- Add Metal ".al" extension to end of file.
    let fullPath = dirPath </> addExtension path "al"
    contents <- readFile fullPath

    Program imports body <- parseContents contents

    let importPath = takeDirectory path
    let fullImports = map (importPath </>) imports

    return (fullImports, body)

-- Evalutes the program, and defaults to accepting if no terminating state is
-- reached.
evalProg :: (MonadOutput m, Monad t) => Tree t -> Program -> Config -> t (App m Config)
evalProg tree (Program imports body) config = do
    imported <- importStms tree imports
    let allStms = imported ++ [body]
    let allComp = compose allStms
    return $ evalStm (Comp allComp Accept) config
