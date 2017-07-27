module Semantics.Bexp (bexpVal) where

import State.App
import State.Config
import Semantics.DerivedSymbol
import Syntax.Tree

notVal :: (Monad m) => Bexp -> Config -> App m Bool
notVal b config = do
    val <- bexpVal b config
    return (not val)

binaryOp :: (Monad m) => (a -> a -> Bool) -> (b -> Config -> App m a) -> b -> b -> Config -> App m Bool
binaryOp op recursiveF x y config = do
    val1 <- recursiveF x config
    val2 <- recursiveF y config
    return (op val1 val2)

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: (Monad m) => Bexp -> Config -> App m Bool
bexpVal (TRUE)  = const (return True)
bexpVal (FALSE) = const (return False)
bexpVal (Not b) = notVal b
bexpVal (And b1 b2) = binaryOp (&&) bexpVal b1 b2
bexpVal (Or b1 b2) = binaryOp (||) bexpVal b1 b2
bexpVal (Eq s1 s2) = binaryOp (==) derivedSymbolVal s1 s2
bexpVal (Le s1 s2) = binaryOp (<=) derivedSymbolVal s1 s2
bexpVal (Ne s1 s2) = binaryOp (/=) derivedSymbolVal s1 s2
