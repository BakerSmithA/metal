module Main where

import Syntax.Tree
import State.Config as Config
import Semantics.Denotational
import Control.Monad.Except

main :: IO ()
main = do
    let stm = (PrintStr "STR")
    let res = evalS stm Config.initial
    runExceptT res >>= printResult
    return ()

-- import Syntax.Tree
-- import Syntax.Parser
-- import Semantics.State
-- import Semantics.Denotational
-- import Text.Megaparsec
-- import Text.Megaparsec.String
-- import System.Environment

-- -- Attempts to parse the specified file.
-- parseFromFile :: Parser a -> FilePath -> IO (Either (ParseError Char Dec) a)
-- parseFromFile p file = runParser p file <$> readFile file
--
-- -- Places `str` on the tape, then interprets `stm`, printing whether the
-- -- program accepted or rejected.
-- runProg :: String -> Stm -> IO ()
-- runProg str stm = do
--     let config = configFromTapeStr str
--     case evalStm stm config of
--         HaltR -> putStrLn "Rejected"
--         HaltA -> putStrLn "Accepted"
--
-- -- Expects the arguments: source file name, tape string i.e. the string placed
-- -- at the start of the tape.
-- main :: IO ()
-- main = do
--     [sourceName, str] <- getArgs
--     parsed            <- parseFromFile stm sourceName
--     case parsed of
--         Left  err  -> putStrLn ("Error: " ++ show err)
--         Right prog -> runProg str prog
