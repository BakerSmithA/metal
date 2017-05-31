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
cond :: (Config -> Bool) -> (Config -> State) -> (Config -> State) -> (Config -> State)
cond p b1 b2 c = if p c then b1 c else b2 c

-- Evaulates a while loop with condition `b` and body `stm`.
while :: Bexp -> Stm -> Config -> State
while b stm = fix f where
    f g = cond (bexpVal b) (\c -> stmVal stm c >>= g) return

-- Evaulates a function declaration by adding the function to the environment.
declFunc :: FuncName -> Stm -> Config -> State
declFunc fName stm (tape, pos, envf) = Inter (tape, pos, envf') where
    envf' = update fName (Just stm) envf

-- Evaulates a function call to the function named `fName`. Once the scope of
-- the function exits, the function environment is returned to what it was
-- before the function call.
callFunc :: FuncName -> Config -> State
callFunc fName c@(tape, pos, envf) = do
    let body = funcBody fName envf
    (tape', pos', _) <- stmVal body c
    return (tape', pos', envf)

-- Evaulates a command to print the symbol under the read/write head.
-- TODO
printRead :: Config -> State
printRead = return

-- Evaulates a command to print the supplied string.
-- TODO
printStr :: String -> Config -> State
printStr s = return

-- The semantic function S[[.]] over statements. The writer returned keeps
-- track of the text output of the statement.
stmVal :: Stm -> Config -> State
stmVal (MoveLeft)           = moveLeft
stmVal (MoveRight)          = moveRight
stmVal (Write s)            = write s
stmVal (Reject)             = \c -> HaltR
stmVal (Accept)             = \c -> HaltA
stmVal (If b stm)           = cond (bexpVal b) (stmVal stm) return
stmVal (IfElse b stm1 stm2) = cond (bexpVal b) (stmVal stm1) (stmVal stm2)
stmVal (While b stm)        = while b stm
stmVal (Func fName body)    = declFunc fName body
stmVal (Call fName)         = callFunc fName
stmVal (Comp stm1 stm2)     = \c -> (stmVal stm1 c) >>= (stmVal stm2)
stmVal (PrintRead)          = printRead
stmVal (PrintStr str)       = printStr str
