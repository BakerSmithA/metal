module Semantics.Bexp (bexpVal) where

import Control.Monad.Except hiding (fix)
import State.App
import State.Config
import Semantics.DerivedSymbol
import Syntax.Tree

notVal :: Bexp -> Config -> App Bool
notVal b config = do
    val <- bexpVal b config
    return (not val)

binaryOp :: (a -> a -> Bool) -> (b -> Config -> App a) -> b -> b -> Config -> App Bool
binaryOp op recursiveF x y config = do
    val1 <- recursiveF x config
    val2 <- recursiveF y config
    return (op val1 val2)

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> Config -> App Bool
bexpVal (TRUE)  = const (return True)
bexpVal (FALSE) = const (return False)
bexpVal (Not b) = notVal b
bexpVal (And b1 b2) = binaryOp (&&) bexpVal b1 b2
bexpVal (Or b1 b2) = binaryOp (||) bexpVal b1 b2
bexpVal (Eq s1 s2) = binaryOp (==) derivedSymbolVal s1 s2
bexpVal (Le s1 s2) = binaryOp (<=) derivedSymbolVal s1 s2
bexpVal (Ne s1 s2) = binaryOp (/=) derivedSymbolVal s1 s2
