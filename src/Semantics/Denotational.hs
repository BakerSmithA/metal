module Semantics.Denotational where

import Syntax.Tree
import Semantics.State
import Control.Monad.Writer

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> Config -> TapeSymbol
derivedSymbolVal (Literal s) st       = s
derivedSymbolVal (Read)     (t, p, e) = t p

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> Config -> Bool
bexpVal (TRUE)      st = True
bexpVal (FALSE)     st = False
bexpVal (Not b)     st = not (bexpVal b st)
bexpVal (And b1 b2) st = (bexpVal b1 st) && (bexpVal b2 st)
bexpVal (Or b1 b2)  st = (bexpVal b1 st) || (bexpVal b2 st)
bexpVal (Eq s1 s2)  st = (derivedSymbolVal s1 st) == (derivedSymbolVal s2 st)
bexpVal (Le s1 s2)  st = (derivedSymbolVal s1 st) <= (derivedSymbolVal s2 st)

-- Updates the value of `f x` to be `r`.
update :: (Eq a) => a -> b -> (a -> b) -> (a -> b)
update x r f x' = if x' == x then r else f x'

-- An auxiliary function for `stmVal` used to the bodies of functions are
-- restored, once a scope is exited, if the body is overwritten inside said
-- scope.
restore :: (Eq a) => [a] -> (a -> b) -> (a -> b) -> (a -> b)
restore xs f f' x = if x `elem` xs then f x else f' x

-- Returns the names of all the functions in a statement. This is an auxiliary
-- function for `stmVal` used when restoring the bodies of overwritten functions.
funcNames :: Stm -> [FuncName]
funcNames (Func fName stm)     = fName : (funcNames stm)
funcNames (If b stm)           = funcNames stm
funcNames (IfElse b stm1 stm2) = (funcNames stm1) ++ (funcNames stm2)
funcNames (While b stm)        = funcNames stm
funcNames (Comp stm1 stm2)     = (funcNames stm1) ++ (funcNames stm2)
funcNames stm                  = []

-- Conditionally chooses to 'execute' `g1` to `g2` based on whether `p`
-- evaulautes to True or False, respectively.
cond :: (Config -> Bool)
     -> (Config -> Config)
     -> (Config -> Config)
     -> (Config -> Config)
cond p g1 g2 st = if p st then g1 st else g2 st

-- Returns 0 if `x` is 0, otherwise returns `x-1`. An auxiliary function used by
--`stmVal` to ensure the read write head does not move left past the zero
-- position.
checkLeft :: Pos -> Pos
checkLeft 0 = 0
checkLeft x = x - 1

-- The semantic function S[[.]] over statements. The writer returned keeps
-- track of the text output of the statement.
stmVal :: Stm -> Config -> ProgState
stmVal = undefined
