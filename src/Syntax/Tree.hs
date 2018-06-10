{-# LANGUAGE GADTs #-}

module Syntax.Tree where

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Types associated with data stored in variables.
data DataType = SymType
              | TapeType
              | CustomType StructName
              deriving (Eq, Show)

-- Types that have a data type, e.g. Symbol, Tape, Struct, etc.
class Typed a where
    typeOf :: a -> DataType

--------------------------------------------------------------------------------
-- Tape Symbols
--------------------------------------------------------------------------------

-- Tape symbol, i.e. a symbol contained in a cell of the machine's tape.
type TapeSymbol = Char

-- Values that evaluate to tape symbols.
data Sym = Read VarName
         | SymLit TapeSymbol
         deriving (Eq, Show)

instance Typed Sym where
    typeOf _ = SymType

--------------------------------------------------------------------------------
-- Tape
--------------------------------------------------------------------------------

-- Values that evaluate to tape references.
data Tape = TapeLit String
          deriving (Eq, Show)

instance Typed Tape where
  typeOf _ = TapeType

--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

type Identifier = String
type SnakeId    = Identifier
type CamelId    = Identifier

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

-- Used to refer to variables, functions, structs, etc.
type VarName = SnakeId

-- Values that evaluate to either a symbol or tape.
data Any = S Sym
         | T Tape
         deriving (Eq, Show)

instance Typed Any where
    typeOf (S s) = typeOf s
    typeOf (T t) = typeOf t

-- Values that can be looked up using a variable, or given as an expression
-- that evaluates to the type.
data Val a = New a
           | Var VarName
           deriving (Eq, Show)

-- Convenience function.
fromSymVal :: Sym -> Val Any
fromSymVal s = New (S s)

-- Convenience function.
fromTapeVal :: Tape -> Val Any
fromTapeVal t = New (T t)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- Name of a function.
type FuncName = SnakeId
-- Name of an argument to the function.
type ArgName = SnakeId
-- Argument supplied when defining the function.
type FuncDeclArg = (ArgName, DataType)
-- Argument supplied when calling the function.
type FuncCallArg = Val Any

-- Returns the type of an argument to a function invocation.
argType :: FuncDeclArg -> DataType
argType = snd

--------------------------------------------------------------------------------
-- Structs
--------------------------------------------------------------------------------

-- Name of a structure.
type StructName = CamelId
-- Variable contained within a struct.
type StructMemberVar = (VarName, DataType)

-- Returns the type of the variable in the struct.
memberVarType :: StructMemberVar -> DataType
memberVarType = snd

--------------------------------------------------------------------------------
-- Bexp
--------------------------------------------------------------------------------

-- Syntax tree for boolean expressions.
data Bexp = TRUE
          | FALSE
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp
          | Eq (Val Sym) (Val Sym)
          | Le (Val Sym) (Val Sym)
          | Ne (Val Sym) (Val Sym)
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Stm
--------------------------------------------------------------------------------

-- Syntax tree for statements.
data Stm = MoveLeft (Val Tape)
         | MoveRight (Val Tape)
         | Write (Val Tape) (Val Sym)
         | WriteStr (Val Tape) [TapeSymbol]
         | Accept
         | Reject
         | If Bexp Stm [(Bexp, Stm)] (Maybe Stm)
         | While Bexp Stm
         | VarDecl VarName (Val Any)
         | FuncDecl FuncName [FuncDeclArg] Stm
         | StructDecl StructName [StructMemberVar]
         | Call FuncName [FuncCallArg]
         | Comp Stm Stm
         | PrintRead (Val Tape)
         | PrintStr String
         | DebugPrintTape (Val Tape)
         deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------

-- Path of a Metal file to be imported.
type ImportPath = String

-- The contents of a metal file.
type FileContents = String
