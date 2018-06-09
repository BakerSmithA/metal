module Syntax.Tree where

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Types associated with data stored in variables.
data DataType = SymType
              | TapeType
              | CustomType StructName
              deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Tape Symbols
--------------------------------------------------------------------------------

-- Tape symbol, i.e. a symbol contained in a cell of the machine's tape.
type TapeSymbol = Char

-- Values that evaluate to tape symbols.
data SymVal = Read VarName
            | SymLit TapeSymbol
            deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Tape
--------------------------------------------------------------------------------

-- Values that evaluate to tape references.
data TapeVal = TapeLit String
               deriving (Eq, Show)

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
data AnyVal = S SymVal
            | T TapeVal
            deriving (Eq, Show)
-- Values that can be looked up using a variable, or given as an expression
-- that evaluates to the type.
data VarVal a = ValExpr a
              | Var VarName
              deriving (Eq, Show)

-- Convenience function.
fromSymVal :: SymVal -> VarVal AnyVal
fromSymVal s = ValExpr (S s)

-- Convenience function.
fromTapeVal :: TapeVal -> VarVal AnyVal
fromTapeVal t = ValExpr (T t)

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
type FuncCallArg = VarVal AnyVal

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
          | Eq (VarVal SymVal) (VarVal SymVal)
          | Le (VarVal SymVal) (VarVal SymVal)
          | Ne (VarVal SymVal) (VarVal SymVal)
          deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Stm
--------------------------------------------------------------------------------

-- Syntax tree for statements.
data Stm = MoveLeft VarName
         | MoveRight VarName
         | Write VarName (VarVal SymVal)
         | WriteStr VarName [TapeSymbol]
         | Accept
         | Reject
         | If Bexp Stm [(Bexp, Stm)] (Maybe Stm)
         | While Bexp Stm
         | VarDecl VarName (VarVal SymVal)
         | TapeDecl VarName (VarVal TapeVal)
         | FuncDecl FuncName [FuncDeclArg] Stm
         | StructDecl StructName [StructMemberVar]
         | Call FuncName [FuncCallArg]
         | Comp Stm Stm
         | PrintRead VarName
         | PrintStr String
         | DebugPrintTape VarName
         deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------

-- Path of a Metal file to be imported.
type ImportPath = String

-- The contents of a metal file.
type FileContents = String
