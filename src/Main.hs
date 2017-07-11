module Main where

import Control.Exception
import Semantics.Stm
import State.App
import State.Config as Config
import State.Error
import State.Machine
import Syntax.Tree
import Syntax.Parser
import System.Environment
import qualified Text.Megaparsec as M

-- Takes in arguments to the program, and returns the parsed arguments, or
-- an error if the arguments were not parsed correctly.
parseArgs :: [String] -> IO (FilePath, [TapeSymbol])
parseArgs [path]       = return (path, [])
parseArgs [path, syms] = return (path, syms)
parseArgs _            = throw (userError "Incorrect number of arguments, expected <source_file> <tape>")

-- Parses the contents of source file, returning either the parsed program, or
-- the parse error.
parseContents :: String -> IO Stm
parseContents contents = do
    let result = M.runParser program "" contents
    either throw (\(Program _ body) -> return body) result

-- Given a program statement, the program is run with an initially
-- empty environment, and tape containing `syms`.
evalSemantics :: Stm -> [TapeSymbol] -> IO (Machine Config)
evalSemantics s syms = do
    let config = Config.fromString syms
    evalApp (evalStm s config)

main :: IO ()
main = do
    args <- getArgs
    (filePath, tapeSyms) <- parseArgs args
    sourceCode <- readFile filePath
    parsedStm <- parseContents sourceCode
    result <- evalSemantics parsedStm tapeSyms
    putStrLn (show result)
