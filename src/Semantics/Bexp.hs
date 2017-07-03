module Semantics.Bexp where

import Control.Monad.Except hiding (fix)
import Semantics.DerivedSymbol
import Semantics.Helpers
import State.App
import Syntax.Tree

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> AppConfig -> App Bool
bexpVal (TRUE)      _ = return True
bexpVal (FALSE)     _ = return False
bexpVal (Not b)     p = liftM not (bexpVal b p)
bexpVal (And b1 b2) p = liftM2 (&&) (bexpVal b1 p) (bexpVal b2 p)
bexpVal (Or b1 b2)  p = liftM2 (||) (bexpVal b1 p) (bexpVal b2 p)
bexpVal (Eq s1 s2)  p = liftM2 (==) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)
bexpVal (Le s1 s2)  p = liftM2 (<=) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)
bexpVal (Ne s1 s2)  p = liftM2 (/=) (derivedSymbolVal s1 p) (derivedSymbolVal s2 p)
