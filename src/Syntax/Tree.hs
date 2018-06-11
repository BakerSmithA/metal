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
data SymExpr = Read TapeExpr
             | SymLit TapeSymbol
             | SymVar VarName
             deriving (Eq, Show)

instance Typed SymExpr where
    typeOf _ = SymType

--------------------------------------------------------------------------------
-- Tape
--------------------------------------------------------------------------------

-- Values that evaluate to tape references.
data TapeExpr = TapeLit String
              | TapeVar VarName
              deriving (Eq, Show)

instance Typed TapeExpr where
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
data AnyValExpr = S SymExpr
                | T TapeExpr
                deriving (Eq, Show)

instance Typed AnyValExpr where
    typeOf (S s) = typeOf s
    typeOf (T t) = typeOf t

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
type FuncCallArg = AnyValExpr

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
          | Eq SymExpr SymExpr
          | Le SymExpr SymExpr
          | Ne SymExpr SymExpr
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Stm
--------------------------------------------------------------------------------

-- Syntax tree for statements.
data Stm = MoveLeft TapeExpr
         | MoveRight TapeExpr
         | Write TapeExpr SymExpr
         | Accept
         | Reject
         | If Bexp Stm [(Bexp, Stm)] (Maybe Stm)
         | While Bexp Stm
         | VarDecl VarName AnyValExpr
         | FuncDecl FuncName [FuncDeclArg] Stm
         | StructDecl StructName [StructMemberVar]
         | Call FuncName [FuncCallArg]
         | Comp Stm Stm
         | PrintRead TapeExpr
         | PrintStr String
         | DebugPrintTape TapeExpr
         deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------

-- Path of a Metal file to be imported.
type ImportPath = String

-- The contents of a metal file.
type FileContents = String
