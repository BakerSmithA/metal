module Syntax.Tree where

-- The type representing a tape symbol, i.e. a symbol contained in a cell of
-- the machine's tape.
type TapeSymbol = Char

-- The type representing a variable name,
type VarName = String

-- The type representing a function name.
type FuncName = String

-- The type representing a function argument name.
type ArgName = String

-- A type that represents all the declared arguments to a function.
type FuncDeclArgs = [ArgName]

-- A type that represents all the arguments passed to a function call.
type FuncCallArgs = [DerivedSymbol]

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
          | Ne DerivedSymbol DerivedSymbol
          deriving (Eq, Show)

-- The type that represents the syntax tree for statements.
data Stm = MoveLeft
         | MoveRight
         | Write DerivedSymbol
         | Accept
         | Reject
         | If Bexp Stm [(Bexp, Stm)] (Maybe Stm)
         | While Bexp Stm
         | VarDecl VarName DerivedSymbol
         | FuncDecl FuncName FuncDeclArgs Stm
         | Call FuncName FuncCallArgs
         | Comp Stm Stm
         | PrintRead
         | PrintStr String
         deriving (Eq, Show)

-- The type that represents a section of a path to a Metal file. For example,
-- in the path "Directory.SubDirectory.FileName" the three path components
-- "Directory", "SubDirectory", and "FileName".
type ImportPathComponent = String

-- The type that represents each component the path of a Metal file to be
-- imported. For example, the path "Directory.SubDirectory.FileName" is
-- represented as ["Directory", "SubDirectory", "FileName"].
type ImportPath = [ImportPathComponent]

-- A type that represents a parsed program.
data Program = Program [ImportPath] Stm deriving (Eq, Show)
