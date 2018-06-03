module Main where

import Control.Exception
import Semantics.Program
import State.App
import State.Config as Config
import State.Machine
import Syntax.Tree
import qualified Syntax.ParseState as S (ParseState, fromList)
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
evalSemantics :: String -> S.ParseState -> String -> Program -> [TapeSymbol] -> IO (Machine Config)
evalSemantics mainTapeName parseEvalState startDir s syms = do
    let config = Config.fromString mainTapeName syms
    app <- evalProg (ioTree parseEvalState startDir) s config
    evalApp app

main :: IO ()
main = do
    args <- getArgs
    (filePath, tapeSyms) <- parseArgs args
    sourceCode <- readFile filePath
    let mainTapeName = "main"
    let parseEvalState = S.fromList [mainTapeName]
    parsedProg <- parseContents filePath parseEvalState sourceCode
    let startDir = takeDirectory filePath
    result <- evalSemantics mainTapeName parseEvalState startDir parsedProg tapeSyms
    putStrLn $ "\n" ++ (show result)
