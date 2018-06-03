module Semantics.Program where

import Control.Exception
import Control.Monad.Error
import Syntax.Tree
import Syntax.ParseState (ParseState)
import Syntax.Parser (importPaths, parseState, parseState', program)
import State.App
import State.Config
import State.Output
import State.Tree
import Semantics.Stm
import System.FilePath
import Text.Megaparsec as M

type FileContents = String
type FileData = (ImportPath, FileContents)
type ImportTree m = Tree m ImportPath FileData

-- Throws an error if parsing is unsucessful.
tryParse :: (Monad m) => Either (ParseError Char Dec) a -> m a
tryParse = either throw return

-- Parses the import statements from the contents of a file, or throws a
-- parse error if unsuccessful.
parseImports :: (MonadError e m) => FilePath -> FileContents -> m [ImportPath]
parseImports sourceFileName contents = tryParse parsed where
    parsed = M.parse importPaths sourceFileName contents

-- Uses the file system to read a Metal file and return any files the read file
-- imports, along with the contents of the file.
ioTree :: FilePath -> ImportPath -> IO ([ImportPath], FileData)
ioTree dirPath importPath = do
    -- Add Metal ".al" extension to end of file.
    let fullPath = dirPath </> addExtension importPath "al"
    contents <- readFile fullPath

    imports <- parseImports fullPath contents

    let importDirPath = takeDirectory importPath
    let fullImports = map (importDirPath </>) imports

    return (fullImports, (fullPath, contents))

-- Combines the dependenices such that variables/functions declared can be
-- used by further files.
-- Places an accept statement at the end of all the files, therefore the program
-- defaults to accepting if not specified otherwise.
foldFiles :: (Monad m) => ParseState -> [FileData] -> m Stm
foldFiles initialState []                          = return Accept
foldFiles initialState ((filePath, contents):rest) = do
    (Program stm, state') <- tryParse $ parseState' initialState program filePath contents
    stms <- foldFiles state' rest
    return (Comp stm stms)

-- Parses the initial file and evaulates it, possibly importing other files too.
evalProg :: (MonadOutput m, Monad t) => ImportTree t -> ImportPath -> ParseState -> Config -> t (App m Config)
evalProg tree filePath initialState config = do
    fileDatas <- dfFlattenTree tree [filePath]
    stm <- foldFiles initialState fileDatas
    return (evalStm stm config)
