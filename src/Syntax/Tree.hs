module Syntax.Tree where

-- Tape symbol, i.e. a symbol contained in a cell of the machine's tape.
type TapeSymbol = Char

-- Can be used as a reference to a piece of memory, e.g. symbol, tape, function.
type Identifier = String

-- Variable name, i.e. reference to a symbol.
type VarName = Identifier

-- Function name.
type FuncName = Identifier

-- Function argument name.
type ArgName = Identifier

-- Struct Name, must start with a capital letter.
type StructName = String

-- Variable contained within a struct.
type StructMemberVar = (VarName, DataType)

memberVarType :: StructMemberVar -> DataType
memberVarType = snd

-- Types that can be passed to functions.
data DataType = SymType
              | TapeType
              | CustomType StructName
              deriving (Eq, Show)

-- Argument to a function.
data FuncDeclArg = FuncDeclArg ArgName DataType deriving (Eq, Show)

-- All the declared arguments to a function.
type FuncDeclArgs = [FuncDeclArg]

-- All the arguments passed to a function call.
type FuncCallArgs = [VarVal AnyVal]

-- Values that evaluate to tape symbols.
data SymVal = Read VarName
            | SymLit TapeSymbol
            deriving (Eq, Show)

-- Values that evaluate to tape references.
data TapeVal = TapeLit String
               deriving (Eq, Show)

-- Values that evaluate to either a symbol or tape.
data AnyVal = S SymVal
            | T TapeVal
            deriving (Eq, Show)

-- Values that can be looked up using a variable, or given as an expression
-- that evaluates to the type.
data VarVal a = ValExpr a
              | Var VarName
              deriving (Eq, Show)

fromSymVal :: SymVal -> VarVal AnyVal
fromSymVal s = ValExpr (S s)

fromTapeVal :: TapeVal -> VarVal AnyVal
fromTapeVal t = ValExpr (T t)

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
         | FuncDecl FuncName FuncDeclArgs Stm
         | StructDecl StructName [StructMemberVar]
         | Call FuncName FuncCallArgs
         | Comp Stm Stm
         | PrintRead VarName
         | PrintStr String
         | DebugPrintTape VarName
         deriving (Eq, Show)

-- Path of a Metal file to be imported.
type ImportPath = String

-- The contents of a metal file.
type FileContents = String
