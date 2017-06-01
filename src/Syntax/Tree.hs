module Syntax.Tree where

-- The type representing a tape symbol, i.e. a symbol contained in a cell of
-- the machine's tape.
type TapeSymbol = Char

-- The type representing a variable name,
type VarName = String

-- The type representing a function name.
type FuncName = String

-- The type represented a derived symbol, i.e. either a literal tape symbol, or
-- a symbol read from under the read/write head.
data DerivedSymbol = Read
                   | Var VarName
                   | Literal TapeSymbol
                   deriving (Eq, Show)

-- The type representing the syntax tree for boolean expressions.
data Bexp = TRUE
          | FALSE
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp
          | Eq DerivedSymbol DerivedSymbol
          | Le DerivedSymbol DerivedSymbol
          deriving (Eq, Show)

-- The type that represents the syntax tree for statements.
data Stm = MoveLeft
         | MoveRight
         | Write DerivedSymbol
         | Reject
         | Accept
         | If Bexp Stm [(Bexp, Stm)] (Maybe Stm)
         | While Bexp Stm
         | Func FuncName Stm
         | Call FuncName
         | Comp Stm Stm
         | PrintRead
         | PrintStr String
         deriving (Eq, Show)
