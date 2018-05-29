module Main where

import Control.Exception
import Semantics.Program
import State.App
import State.Config as Config
import State.Machine
import State.Tape
import Syntax.Tree
import System.Environment
import System.FilePath

-- Takes in arguments to the program, and returns the parsed arguments, or
-- an error if the arguments were not parsed correctly.
parseArgs :: [String] -> IO (FilePath, [TapeSymbol])
parseArgs [path]       = return (path, [])
parseArgs [path, syms] = return (path, syms)
parseArgs _            = throw (userError "Incorrect number of arguments, expected <source_file> <tape>")

-- Given a program statement, the program is run with an initially
-- empty environment, and tape containing `syms`.
evalSemantics :: String -> Program -> [TapeSymbol] -> IO (Machine Tape Config)
evalSemantics startDir s syms = do
    let config = Config.fromString syms
    app <- evalProg (ioTree startDir) s config
    evalApp app

main :: IO ()
main = do
    args <- getArgs
    (filePath, tapeSyms) <- parseArgs args
    sourceCode <- readFile filePath
    parsedProg <- parseContents sourceCode
    let startDir = takeDirectory filePath
    result <- evalSemantics startDir parsedProg tapeSyms
    putStrLn (show result)
