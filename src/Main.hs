module Main where

import Syntax.Tree
import Syntax.Parser
import Semantics.State
import Semantics.Denotational
import Text.Megaparsec
import Text.Megaparsec.String
import System.Environment

-- Attempts to parse the specified file.
parseFromFile :: Parser a -> FilePath -> IO (Either (ParseError Char Dec) a)
parseFromFile p file = runParser p file <$> readFile file

-- Expects the arguments: source file name, tape string i.e. the string placed
-- at the start of the tape.
main :: IO ()
main = do
    [sourceName, str] <- getArgs
    parsed            <- parseFromFile stm sourceName

    case parsed of
        Left  err  -> putStrLn ("Error: " ++ show err)
        Right prog -> putStrLn $ show prog
