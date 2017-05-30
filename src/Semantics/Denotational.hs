module Semantics.Denotational where

import Syntax.Tree
import Semantics.State

-- The semantic function D[[.]] over tape symbols.
derivedSymbolVal :: DerivedSymbol -> Config -> TapeSymbol
derivedSymbolVal (Literal s) st               = s
derivedSymbolVal (Read)     (tape, pos, envf) = tape pos

-- The semantic function B[[.]] over boolean expressions.
bexpVal :: Bexp -> Config -> Bool
bexpVal (TRUE)      st = True
bexpVal (FALSE)     st = False
bexpVal (Not b)     st = not (bexpVal b st)
bexpVal (And b1 b2) st = (bexpVal b1 st) && (bexpVal b2 st)
bexpVal (Or b1 b2)  st = (bexpVal b1 st) || (bexpVal b2 st)
bexpVal (Eq s1 s2)  st = (derivedSymbolVal s1 st) == (derivedSymbolVal s2 st)
bexpVal (Le s1 s2)  st = (derivedSymbolVal s1 st) <= (derivedSymbolVal s2 st)

-- Fixpoint operator used to defined loops.
fix :: (a -> a) -> a
fix f = let x = f x in x

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

-- Evauluates moving the read/write head left, ensuring the head doesn't move
-- past the zero position.
moveLeft :: Config -> State
moveLeft (tape, pos, envf) = Inter (tape, check pos, envf) where
    check 0 = 0
    check x = x - 1

-- Evaluates moving the read/write head right.
moveRight :: Config -> State
moveRight (tape, pos, envf) = Inter (tape, pos + 1, envf)

-- Evaualtes writing `s` at the position of the read/write head.
write :: TapeSymbol -> Config -> State
write s (tape, pos, envf) = Inter (tape', pos, envf) where
    tape' = update pos s tape

-- Conditionally chooses to 'execute' `g1` to `g2` based on whether `p`
-- evaulautes to True or False, respectively.
cond :: (Config -> Bool) -> (State -> State) -> (State -> State) -> (State -> State)
cond p b1 b2 st = do
    c <- st
    if p c then b1 st else b2 st

-- Evaulates a while loop with condition `b` and body `stm`.
while :: Bexp -> Stm -> State -> State
while b stm = fix f where
    f g = cond (bexpVal b) (g . (stmVal stm)) id

-- Evaulates a function declaration by adding the function to the environment.
declFunc :: FuncName -> Stm -> Config -> State
declFunc fName stm (tape, pos, envf) = Inter (tape, pos, envf') where
    envf' = update fName stm envf

-- Evaulates a function call to the function named `fName`. Once the scope of
-- the function exits, the function environment is returned to what it was
-- before the function call.
callFunc :: FuncName -> Config -> State
callFunc fName (tape, pos, envf) = undefined

-- The semantic function S[[.]] over statements. The writer returned keeps
-- track of the text output of the statement.
stmVal :: Stm -> State -> State
stmVal (MoveLeft)           st = st >>= moveLeft
stmVal (MoveRight)          st = st >>= moveRight
stmVal (Write s)            st = st >>= write s
stmVal (Reject)             st = HaltR
stmVal (Accept)             st = HaltA
stmVal (If b stm)           st = (cond (bexpVal b) (stmVal stm) id) st
stmVal (IfElse b stm1 stm2) st = (cond (bexpVal b) (stmVal stm1) (stmVal stm2)) st
stmVal (While b stm)        st = while b stm st
stmVal (Func fName stm)     st = st >>= (declFunc fName stm)
stmVal (Call fName)         st = undefined
