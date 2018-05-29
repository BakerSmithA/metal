module Syntax.Tree where

-- Tape symbol, i.e. a symbol contained in a cell of the machine's tape.
type TapeSymbol = Char

-- Variable name, i.e. reference to a symbol.
type VarName = String

-- Tape name, i.e. a reference to a tape.
type TapeName = String

-- Function name.
type FuncName = String

-- Function argument name.
type ArgName = String

-- All the declared arguments to a function.
type FuncDeclArgs = [ArgName]

-- All the arguments passed to a function call.
type FuncCallArgs = [DerivedSymbol]

-- Derived symbol, i.e. either a literal tape symbol, or a symbol read from
-- under the read/write head.
data DerivedSymbol = Read TapeName
                   | Var VarName
                   | Literal TapeSymbol
                   deriving (Eq, Show)

-- Syntax tree for boolean expressions.
data Bexp = TRUE
          | FALSE
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp
          | Eq DerivedSymbol DerivedSymbol
          | Le DerivedSymbol DerivedSymbol
          | Ne DerivedSymbol DerivedSymbol
          deriving (Eq, Show)

-- Syntax tree for statements.
data Stm = MoveLeft TapeName
         | MoveRight TapeName
         | Write DerivedSymbol TapeName
         | WriteStr [TapeSymbol] TapeName
         | Accept
         | Reject
         | If Bexp Stm [(Bexp, Stm)] (Maybe Stm)
         | While Bexp Stm
         | VarDecl VarName DerivedSymbol
         | TapeDecl TapeName String
         | FuncDecl FuncName FuncDeclArgs Stm
         | Call FuncName FuncCallArgs
         | Comp Stm Stm
         | PrintRead TapeName
         | PrintStr String
         deriving (Eq, Show)

-- Path of a Metal file to be imported.
type ImportPath = String

-- A type that represents a parsed program.
data Program = Program [ImportPath] Stm deriving (Eq, Show)
