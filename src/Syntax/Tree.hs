{-# LANGUAGE GADTs #-}

module Syntax.Tree where

--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

type Identifier = String
type SnakeId    = Identifier
type CamelId    = Identifier

-- Used to refer to variables, functions, structs, etc.
type VarName = SnakeId
-- Represents how to 'reach' a variable, e.g. ["x", "m_x"] would be the
-- variable m_x, via the object x. The code would be: x.m_x
type VarPath = [VarName]

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Types associated with data stored in variables.
data DataType = SymType
              | TapeType
              | CustomType StructName
              deriving (Eq, Show)

-- Returns whether the data type is a custom type. Says nothing about which
-- custom type it is.
isCustomType :: DataType -> Bool
isCustomType (CustomType _) = True
isCustomType _              = False

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
             | SymVar VarPath
             deriving (Eq, Show)

instance Typed SymExpr where
    typeOf _ = SymType

--------------------------------------------------------------------------------
-- Tape
--------------------------------------------------------------------------------

-- Values that evaluate to tape references.
data TapeExpr = TapeLit String
              | TapeVar VarPath
              deriving (Eq, Show)

instance Typed TapeExpr where
  typeOf _ = TapeType

--------------------------------------------------------------------------------
-- Objects
--------------------------------------------------------------------------------

-- Values that evaluate to structure instances.
data ObjExpr = NewObj StructName [NewObjArg]
             | ObjVar StructName VarPath
             deriving (Eq, Show)

instance Typed ObjExpr where
    typeOf (NewObj structName _) = CustomType structName
    typeOf (ObjVar structName _) = CustomType structName

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

-- Values that evaluate to either a symbol or tape.
data AnyValExpr = S SymExpr
                | T TapeExpr
                | C ObjExpr
                deriving (Eq, Show)

instance Typed AnyValExpr where
    typeOf (S s) = typeOf s
    typeOf (T t) = typeOf t
    typeOf (C c) = typeOf c

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- Name of a function.
type FuncName = SnakeId
-- Name of an argument to a function.
type ArgName = SnakeId
-- Argument supplied when defining a function.
type FuncDeclArg = (ArgName, DataType)
-- Argument supplied when invoking a function.
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
-- Argument supplied when creating an object.
type NewObjArg = AnyValExpr

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
         | Call FuncName [AnyValExpr]
         | StructDecl StructName [StructMemberVar]
         | Comp Stm Stm
         | Print SymExpr
         | PrintLn (Maybe SymExpr)
         | DebugPrintTape TapeExpr
         deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------

-- Path of a Metal file to be imported.
type ImportPath = String

-- The contents of a metal file.
type FileContents = String
