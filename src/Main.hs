module Main where

import Control.Exception
import Semantics.Program
import State.App
import State.Config as Config
import Syntax.Tree
import qualified Syntax.ParseState as S (fromList)
import System.Environment
import System.FilePath

-- Takes in arguments to the program, and returns the parsed arguments, or
-- an error if the arguments were not parsed correctly.
parseArgs :: [String] -> IO (FilePath, [TapeSymbol])
parseArgs [path]       = return (path, [])
parseArgs [path, syms] = return (path, syms)
parseArgs _            = throw (userError "Incorrect number of arguments, expected <source_file> <tape>")

main :: IO ()
main = do
    args <- getArgs
    (filePath, tapeSyms) <- parseArgs args
    let mainTapeName = "main"
    let parseState = S.fromList [mainTapeName]
    let config = Config.fromString mainTapeName tapeSyms
    let startDir = takeDirectory filePath
    app <- evalProg (ioTree startDir) filePath parseState config
    result <- evalApp app
    putStrLn $ "\n" ++ (show result)
