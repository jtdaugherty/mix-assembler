module System.MIX.Symbolic where

-- The type of symbolic MIXAL instructions.  These include symbolic
-- names for instruction locations, symbolic references, literal
-- constants, assembler directives (ORIG, CON, etc.).

data MIXALStmt
    = Dir Directive
    | Inst (Maybe Symbol) OpCode Address (Maybe Index) (Maybe Field)
      deriving (Eq, Show)

data Address = NoAddr
             | AddrExpr Expr
             | AddrRef Symbol
             | AddrLiteral WValue
               deriving (Eq, Show)

data WValue = WValue [(Expr, Field)]
              deriving (Eq, Show)

newtype Index = Index Int
    deriving (Eq, Show)

data Field = Default
           | FieldExpr Expr
    deriving (Eq, Show)

data AtomicExpr = Num Int
                | Sym Symbol
                | Asterisk
                  deriving (Eq, Show)

data Expr = AtExpr AtomicExpr
          | Signed Bool AtomicExpr
          | BinOp Expr BinOp AtomicExpr
            deriving (Eq, Show)

data BinOp = Add
           | Subtract
           | Multiply
           | Divide
           | Frac
           | Field
             deriving (Eq, Show)

data Directive
    = ORIG Expr
    | EQU Symbol Expr
      deriving (Eq, Show)

data OpCode
    = LDR
    | ADD
      deriving (Eq, Show)

newtype Symbol = Symbol String
    deriving (Eq, Show)
