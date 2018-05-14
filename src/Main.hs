module Main where

import Control.Exception
import Semantics.Program
import State.App
import State.Config as Config
import State.Machine
import State.Tape
import Syntax.Tree
import System.Environment

-- Takes in arguments to the program, and returns the parsed arguments, or
-- an error if the arguments were not parsed correctly.
parseArgs :: [String] -> IO (FilePath, [TapeSymbol])
parseArgs [path]       = return (path, [])
parseArgs [path, syms] = return (path, syms)
parseArgs _            = throw (userError "Incorrect number of arguments, expected <source_file> <tape>")

-- Given a program statement, the program is run with an initially
-- empty environment, and tape containing `syms`.
evalSemantics :: Program -> [TapeSymbol] -> IO (Machine Tape Config)
evalSemantics s syms = do
    let config = Config.fromString syms
    app <- evalProg ioTree s config
    evalApp app

main :: IO ()
main = do
    args <- getArgs
    (filePath, tapeSyms) <- parseArgs args
    sourceCode <- readFile filePath
    parsedProg <- parseContents sourceCode
    result <- evalSemantics parsedProg tapeSyms
    putStrLn (show result)
