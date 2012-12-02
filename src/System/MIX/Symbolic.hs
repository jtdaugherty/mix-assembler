{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.MIX.Symbolic where

-- The type of symbolic MIXAL instructions.  These include symbolic
-- names for instruction locations, symbolic references, literal
-- constants, assembler directives (ORIG, CON, etc.).

newtype MIXChar = MIXChar Char
    deriving (Eq, Show)

data MIXALStmt
    = Orig (Maybe DefinedSymbol) WValue
    | Equ (Maybe DefinedSymbol) WValue
    | Con (Maybe DefinedSymbol) WValue
    | Alf (Maybe DefinedSymbol) (MIXChar, MIXChar, MIXChar, MIXChar, MIXChar)
    | Inst (Maybe DefinedSymbol) OpCode (Maybe Address) (Maybe Index) (Maybe Field)
    | End (Maybe DefinedSymbol) WValue
      deriving (Eq, Show)

data Address = AddrExpr Expr
             | AddrRef SymbolRef
             | AddrLiteral WValue
             | LitConst WValue
               deriving (Eq, Show)

data DefinedSymbol = DefNormal Symbol
                   | DefLocal Int
                     deriving (Eq, Show)

data SymbolRef = RefNormal Symbol
               | RefBackward Int
               | RefForward Int
                 deriving (Eq, Show)

data WValue = WValue Expr (Maybe Field) [(Expr, Maybe Field)]
              deriving (Eq, Show)

newtype Index = Index Int
    deriving (Eq, Show)

data Field = FieldExpr Expr
    deriving (Eq, Show)

data AtomicExpr = Num Int
                | Sym SymbolRef
                | Asterisk
                  deriving (Eq, Show)

data Expr = AtExpr AtomicExpr
          | Signed Bool AtomicExpr
          | BinOp Expr [(BinOp, Expr)]
            deriving (Eq, Show)

data BinOp = Add
           | Subtract
           | Multiply
           | Divide
           | Frac
           | Field
             deriving (Eq, Show)

data OpCode
    = LDA | LDX | LD1 | LD2 | LD3 | LD4 | LD5 | LD6
    | LDAN | LDXN | LD1N | LD2N | LD3N | LD4N | LD5N | LD6N
    | STA | STX | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | STJ | STZ
    | ADD | SUB | MUL | DIV
    | ENTA | ENTX | ENT1 | ENT2 | ENT3 | ENT4 | ENT5 | ENT6
    | ENNA | ENNX | ENN1 | ENN2 | ENN3 | ENN4 | ENN5 | ENN6
    | INCA | INCX | INC1 | INC2 | INC3 | INC4 | INC5 | INC6
    | DECA | DECX | DEC1 | DEC2 | DEC3 | DEC4 | DEC5 | DEC6
    | CMPA | CMPX | CMP1 | CMP2 | CMP3 | CMP4 | CMP5 | CMP6
    | JMP | JSJ | JOV | JNOV | JL | JE | JG | JGE | JNE | JLE
    | JAN | JAZ | JAP | JANN | JANZ | JANP
    | JXN | JXZ | JXP | JXNN | JXNZ | JXNP
    | J1N | J1Z | J1P | J1NN | J1NZ | J1NP
    | J2N | J2Z | J2P | J2NN | J2NZ | J2NP
    | J3N | J3Z | J3P | J3NN | J3NZ | J3NP
    | J4N | J4Z | J4P | J4NN | J4NZ | J4NP
    | J5N | J5Z | J5P | J5NN | J5NZ | J5NP
    | J6N | J6Z | J6P | J6NN | J6NZ | J6NP
    | IN | OUT | IOC | JRED | JBUS
    | NUM | CHAR
    | SLA | SRA | SLAX | SRAX | SLC | SRC
    | MOVE | NOP | HLT
      deriving (Eq, Show)

newtype Symbol = Symbol String
    deriving (Eq, Show)
