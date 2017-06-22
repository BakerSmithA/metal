module Main where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Semantics.Denotational
import State.Config as Config
import State.Env as Env
import State.Error
import State.Machine
import State.Program
import Syntax.Tree
import Syntax.Parser
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.String

-- From arguments supplied to the program, retrieves the file name
-- containing the source code, and any symbols to put on the tape.
-- If no tape is supplied, a default tape containing nothing will
-- be used.
getProgArgs :: ExceptT String IO (FilePath, [TapeSymbol])
getProgArgs = do
    args <- liftIO $ getArgs
    case args of
        [path]       -> return (path, [])
        [path, syms] -> return (path, syms)
        _            -> throwError "Incorrect arguments, expected <source_file> <tape>"

-- Parses a source file.
parseFile :: FilePath -> ExceptT String IO Stm
parseFile path = do
    contents <- liftIO (readFile path)
    let parsed = runParser stm "" contents
    either (throwError . show) return parsed

-- Retrieves the arguments from the command line for the source file path and
-- tape symbols. Then parses the source file.
parseArgs :: ExceptT String IO (Stm, [TapeSymbol])
parseArgs = do
    (path, syms) <- getProgArgs
    stm <- parseFile path
    return (stm, syms)

-- Given a program statement, the program is run with an initially
-- empty environment, and tape containing `syms`.
evalSemantics :: Stm -> [TapeSymbol] -> IO (Either RuntimeError (Machine Config))
evalSemantics s syms = do
    let initial = return (Config.fromString syms)
    runProgram (evalStm s initial) (Env.empty)

-- Given a termnated program (this included runtime errors), the end result is
-- printed.
printHalt :: Either RuntimeError (Machine Config) -> IO ()
printHalt = putStrLn . (either (showMsg "Error: ") (showMsg "Halted: ")) where
    showMsg msg x = msg ++ (show x)

-- Evaulates the semantics of the specified statement, with the specified tape.
-- Upton halting, it is printed whether the machine rejected or accepted.
run :: (Stm, [TapeSymbol]) -> IO ()
run (s, syms) = do
    result <- evalSemantics s syms
    printHalt result

main :: IO ()
main = do
    parsed <- runExceptT parseArgs
    either (putStrLn . show) run parsed
