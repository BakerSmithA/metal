module Main where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Typeable
import Semantics.Denotational
import State.Config as Config
import State.Error
import State.Machine
import State.Program
import Syntax.Tree
import Syntax.Parser
import System.Environment
import qualified Text.Megaparsec as M
import Text.Megaparsec.String

-- Errors that can occur during parsing or runtime.
data ProgError = ArgError String
               | ParseError (M.ParseError (M.Token String) M.Dec)
               | SemanticError RuntimeError
               deriving (Typeable)

instance Show ProgError where
    show (ArgError err)      = err
    show (ParseError err)    = show err
    show (SemanticError err) = show err

-- The arguments we want to parse from the command line is the path of the
-- source code file, and any symbols to be placed at the start of the tape.
type Args = (FilePath, [TapeSymbol])

-- Takes in arguments to the program, and returns the parsed arguments, or
-- an error if the arguments were not parsed correctly.
parseArgs :: [String] -> Either ProgError Args
parseArgs [path, tape] = return (path, tape)
parseArgs _            = throwError (ArgError "Incorrect number of arguments, expected <source_file> <tape>")

-- Parses the contents of source file, returning either the parsed program, or
-- the parse error.
parseContents :: String -> Either ProgError Stm
parseContents contents = do
    let result = M.runParser stm "" contents
    either (throwError . ParseError) return result

-- Given a program statement, the program is run with an initially
-- empty environment, and tape containing `syms`.
evalSemantics :: Stm -> [TapeSymbol] -> Either ProgError (Machine Config)
evalSemantics s syms = do
    let config = return (Config.fromString syms)
        result = runProgram (evalStm s config)
    either (throwError . SemanticError) return result

-- Parses `contents`, and runs the parsed statement with `syms` at the start of
-- the tape. If parsing is unsuccessful, or a runtime error occurs, an error
-- is returned.
run ::  String -> [TapeSymbol] -> Either ProgError (Machine Config)
run contents syms = do
    s <- parseContents contents
    evalSemantics s syms

main :: IO ()
main = do
    args <- getArgs
    either printErr printResult (parseArgs args) where
        printErr = putStrLn . show
        printResult (path, syms) = do
            contents <- readFile path
            let result = run contents syms
            putStrLn (show result)
