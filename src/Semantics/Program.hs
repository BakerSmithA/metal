module Semantics.Program where

import Control.Exception
import Control.Monad.Error
import Syntax.Tree
import Syntax.ParseState (ParseState)
import Syntax.Parser (importPaths)
import State.App
import State.Config
import State.Output
import Semantics.Stm
import System.FilePath
import Text.Megaparsec as M
--
-- -- Parses the contents of source file, returning either the parsed program, or
-- -- the parse error.
-- parseContents :: String -> ParseState -> String -> IO Program
-- parseContents sourceFileName initialState contents = do
--     let result = parseState initialState program sourceFileName contents
--     either throw return result

-- Describes a path in the tree, this could represent a file system path.
type Tree m = (ImportPath -> m ([ImportPath], String))

-- Parses the import statements from the contents of a file, or throws a
-- parse error if unsuccessful.
parseImports :: (MonadError e m) => FilePath -> String -> m [ImportPath]
parseImports sourceFileName contents = do
    let result = M.parse importPaths sourceFileName contents
    either throw return result

-- Uses the file system to read a Metal file and return any files the read file
-- imports, along with the contents of the file.
ioTree :: FilePath -> ImportPath -> IO ([ImportPath], String)
ioTree dirPath importPath = do
    -- Add Metal ".al" extension to end of file.
    let fullPath = dirPath </> addExtension importPath "al"
    contents <- readFile fullPath

    imports <- parseImports fullPath contents

    let importDirPath = takeDirectory importPath
    let fullImports = map (importDirPath </>) imports

    return (fullImports, contents)

-- Perfoms a DF search resolving imports into the conents in those files.
-- `tree` describes the shape of the tree by giving a list of branches
-- (import statements) in that file.
importStms :: (Monad m) => Tree m -> [ImportPath] -> m [String]
importStms _ [] = return []
importStms tree (path:rest) = do
    (imports, body) <- tree path
    childrenStms <- importStms tree imports
    restStms <- importStms tree rest
    return (childrenStms ++ [body] ++ restStms)

-- importStms _ [] = return []
-- importStms tree (path:rest) = do
--     (imports, body) <- tree path
--     childrenStms <- importStms tree imports
--     restStms <- importStms tree rest
--     return (childrenStms ++ [body] ++ restStms)

-- ioTree dirPath path = do
--     -- Add Metal ".al" extension to end of file.
--     let fullPath = dirPath </> addExtension path "al"
--     contents <- readFile fullPath
--
--     -- Program imports body <- parseContents fullPath parseState contents
--     imports <- parse
--
--     let importPath = takeDirectory path
--     let fullImports = map (importPath </>) imports
--
--     return (fullImports, body)

-- Evalutes the program, and defaults to accepting if no terminating state is
-- reached.
-- evalProg :: (MonadOutput m, Monad t) => Tree t -> Program -> Config -> t (App m Config)
-- evalProg tree (Program imports body) config = do
--     imported <- importStms tree imports
--     let allStms = imported ++ [body]
--     let allComp = compose allStms
--     return $ evalStm (Comp allComp Accept) config
