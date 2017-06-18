module Main where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Semantics.Denotational
import State.Env as Env
import State.Config as Config
import State.Program
import Syntax.Tree
import Syntax.Parser
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.String

-- type Prog a     = ReaderT Env (ExceptT RuntimeError (MachineT IO)) a

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
parseFile :: FilePath -> ExceptT (ParseError Char Dec) IO Stm
parseFile path = do
    contents <- liftIO (readFile path)
    let parsed = runParser stm "" contents
    either throwError return parsed

-- Given a program statement, the program is run with an initially
-- empty environment, and tape containing `syms`.
evalSemantics :: [TapeSymbol] -> Stm -> IO Config
evalSemantics syms s = undefined

main :: IO ()
main = undefined

    -- [sourceName, str] <- getArgs
    -- parsed            <- parseFromFile stm sourceName
    -- either (putStrLn . show) runProg parsed
