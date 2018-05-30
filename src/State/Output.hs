module State.Output where

class Monad m => MonadOutput m where
    output :: String -> m ()

instance MonadOutput IO where
    -- output :: String -> IO ()
    output str = putStr (read $ "\"" ++ str ++ "\"") -- Account for escaped characters.
