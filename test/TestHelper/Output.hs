{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestHelper.Output where

import Control.Monad.Writer
import State.Output

-- Created with help from:
-- https://lexi-lambda.github.io/blog/2016/10/03/using-types-to-unit-test-in-haskell/

newtype TestM a = TestM (Writer [String] a)
  deriving (Functor, Applicative, Monad, MonadWriter [String])

logTestM :: TestM a -> [String]
logTestM (TestM w) = execWriter w

instance MonadOutput TestM where
    -- output :: String -> TestM ()
    output str = tell [str]
