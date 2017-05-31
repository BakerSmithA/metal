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

-- Evauluates moving the read/write head left, ensuring the head doesn't move
-- past the zero position.
evalMoveLeft :: Config -> State
evalMoveLeft (tape, pos, envf) = Inter (tape, check pos, envf) where
    check 0 = 0
    check x = x - 1

-- Evaluates moving the read/write head right.
evalMoveRight :: Config -> State
evalMoveRight (tape, pos, envf) = Inter (tape, pos + 1, envf)

-- Evaualtes writing `s` at the position of the read/write head.
evalWrite :: TapeSymbol -> Config -> State
evalWrite s (tape, pos, envf) = Inter (tape', pos, envf) where
    tape' = update pos s tape

-- Conditionally chooses to 'execute' a branch if associated predicate
-- evaluates to true. Returns the branch to execute, or `id` if no predicates
-- evaluate to true.
cond :: [(Config -> Bool, Config -> State)] -> (Config -> State)
cond []               c = return c
cond ((p, branch):bs) c = if p c then branch c else cond bs c

-- Evaluates an if-else statement.
evalIf :: Bexp -> Stm -> [(Bexp, Stm)] -> Config -> State
evalIf b stm ps = cond bs where
    bs                    = map makeBranch ((b, stm):ps)
    makeBranch (b', stm') = (bexpVal b', evalStm stm')

-- Evaulates a while loop with condition `b` and body `stm`.
evalWhile :: Bexp -> Stm -> Config -> State
evalWhile b stm = fix f where
    f g = cond [(bexpVal b, \c -> evalStm stm c >>= g)]

-- Evaulates a function declaration by adding the function to the environment.
evalDeclFunc :: FuncName -> Stm -> Config -> State
evalDeclFunc fName stm (tape, pos, envf) = Inter (tape, pos, envf') where
    envf' = update fName (Just stm) envf

-- Evaulates a function call to the function named `fName`. Once the scope of
-- the function exits, the function environment is returned to what it was
-- before the function call.
evalCallFunc :: FuncName -> Config -> State
evalCallFunc fName c@(tape, pos, envf) = do
    let body = funcBody fName envf
    (tape', pos', _) <- evalStm body c
    return (tape', pos', envf)

-- Evaulates a command to print the symbol under the read/write head.
-- TODO
evalPrintRead :: Config -> State
evalPrintRead = return

-- Evaulates a command to print the supplied string.
-- TODO
evalPrintStr :: String -> Config -> State
evalPrintStr s = return

-- The semantic function S[[.]] over statements. The writer returned keeps
-- track of the text output of the statement.
evalStm :: Stm -> Config -> State
evalStm (MoveLeft)           = evalMoveLeft
evalStm (MoveRight)          = evalMoveRight
evalStm (Write s)            = evalWrite s
evalStm (Reject)             = \c -> HaltR
evalStm (Accept)             = \c -> HaltA
evalStm (If b stm cs)        = evalIf b stm cs
evalStm (While b stm)        = evalWhile b stm
evalStm (Func fName body)    = evalDeclFunc fName body
evalStm (Call fName)         = evalCallFunc fName
evalStm (Comp stm1 stm2)     = \c -> (evalStm stm1 c) >>= (evalStm stm2)
evalStm (PrintRead)          = evalPrintRead
evalStm (PrintStr str)       = evalPrintStr str
