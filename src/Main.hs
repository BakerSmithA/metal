module Main where

import Syntax.Tree
import Semantics.Denotational
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.String

-- There are two stages in which errors could occur: parsing and
-- during runtime (semantic).
data Error a b = ParseError a    -- An error whilst parsing code.
               | SemanticError b -- An error whilst running the program.


-- type Prog a     = ReaderT Env (ExceptT RuntimeError (MachineT IO)) a

-- Attempts to parse the specified file.
parseFromFile :: Parser a -> FilePath -> IO (Either (ParseError Char Dec) a)
parseFromFile p file = runParser p file <$> readFile file

runProg :: Stm -> IO Config
runProg = undefined

main :: IO ()
main = do
    [sourceName, str] <- getArgs
    parsed            <- parseFromFile stm sourceName
