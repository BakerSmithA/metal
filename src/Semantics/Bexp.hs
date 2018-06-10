module Semantics.Bexp (bexpVal) where

import State.App
import State.Config
import Semantics.Variable
import Syntax.Tree

notVal :: (Monad m) => Bexp -> Config -> App m (Bool, Config)
notVal b c = do
    (val, c') <- bexpVal b c
    return (not val, c')

binaryOp :: (Monad m) => (a -> a -> Bool) -> (b -> Config -> App m (a, Config)) -> b -> b -> Config -> App m (Bool, Config)
binaryOp op recursiveF x y c1 = do
    (val1, c2) <- recursiveF x c1
    (val2, c3) <- recursiveF y c2
    return (op val1 val2, c3)

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: (Monad m) => Bexp -> Config -> App m (Bool, Config)
bexpVal (TRUE)      c = return (True, c)
bexpVal (FALSE)     c = return (False, c)
bexpVal (Not b)     c = notVal b c
bexpVal (And b1 b2) c = binaryOp (&&) bexpVal b1 b2 c
bexpVal (Or b1 b2)  c = binaryOp (||) bexpVal b1 b2 c
bexpVal (Eq s1 s2)  c = binaryOp (==) symVal s1 s2 c
bexpVal (Le s1 s2)  c = binaryOp (<=) symVal s1 s2 c
bexpVal (Ne s1 s2)  c = binaryOp (/=) symVal s1 s2 c
