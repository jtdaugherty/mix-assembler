{-# OPTIONS_GHC -fno-warn-orphans #-}
module MIX.Assembler.IO
    ( writeProgram
    , readProgram
    )
where

import Control.Applicative ((<$>), (<*>))
import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS
import System.IO (Handle)

import Language.MIXAL.AST
import MIX.Assembler.MIXWord
import MIX.Assembler
    ( Program(..)
    , DebugData(..)
    )

writeProgram :: Handle -> Program -> IO ()
writeProgram h p = BS.hPutStr h $ encode p

readProgram :: Handle -> IO (Either String Program)
readProgram h = decode <$> BS.hGetContents h

putI :: Word8 -> Put
putI = put

getI :: Get Word8
getI = get

instance Serialize MIXWord where
    get = toWord <$> get
    put = put . toInt

instance Serialize Symbol where
    get = Symbol <$> get
    put (Symbol s) = put s

instance Serialize MIXALStmt where
    get = do
      c <- getI
      case c of
        0 -> Orig <$> get <*> get
        1 -> Equ <$> get <*> get
        2 -> Con <$> get <*> get
        3 -> Alf <$> get <*> get
        4 -> Inst <$> get <*> get <*> get <*> get <*> get
        5 -> End <$> get <*> get
        _ -> fail "Invalid MIXAL statement"

    put (Orig ms wv) = putI 0 >> put ms >> put wv
    put (Equ ms wv) = putI 1 >> put ms >> put wv
    put (Con ms wv) = putI 2 >> put ms >> put wv
    put (Alf ms chs) = putI 3 >> put ms >> put chs
    put (Inst ms op ma mi mf) = putI 4 >> put ms >> put op >>
                                put ma >> put mi >> put mf
    put (End ms wv) = putI 5 >> put ms >> put wv

instance Serialize Field where
    get = FieldExpr <$> get
    put (FieldExpr e) = put e

instance Serialize Index where
    get = Index <$> get
    put (Index i) = put i

instance Serialize Address where
    get = do
      c <- getI
      case c of
        0 -> AddrExpr <$> get
        1 -> AddrRef <$> get
        2 -> AddrLiteral <$> get
        3 -> LitConst <$> get
        _ -> fail "Invalid Address"

    put (AddrExpr e) = putI 0 >> put e
    put (AddrRef r) = putI 1 >> put r
    put (AddrLiteral wv) = putI 2 >> put wv
    put (LitConst wv) = putI 3 >> put wv

instance Serialize OpCode where
    get = read <$> get
    put = put . show

instance Serialize SymbolRef where
    get = do
      c <- getI
      case c of
        0 -> RefNormal <$> get
        1 -> RefBackward <$> get
        2 -> RefForward <$> get
        _ -> fail "Invalid symbolref"

    put (RefNormal s) = putI 0 >> put s
    put (RefBackward i) = putI 1 >> put i
    put (RefForward i) = putI 2 >> put i

instance Serialize WValue where
    get = WValue <$> get <*> get <*> get
    put (WValue e mf es) = put e >> put mf >> put es

instance Serialize Expr where
    get = do
      c <- getI
      case c of
        0 -> AtExpr <$> get
        1 -> Signed <$> get <*> get
        2 -> BinOp <$> get <*> get <*> get <*> get
        _ -> fail "Invalid expr"

    put (AtExpr ae) = putI 0 >> put ae
    put (Signed b ae) = putI 1 >> put b >> put ae
    put (BinOp e1 op e2 rest) = putI 2 >> put e1 >> put op >>
                                put e2 >> put rest

instance Serialize BinOp where
    get = read <$> get
    put = put . show

instance Serialize AtomicExpr where
    get = do
      c <- getI
      case c of
        0 -> Num <$> get
        1 -> Sym <$> get
        2 -> return Asterisk
        _ -> fail "Invalid atomicexpr"

    put (Num i) = putI 0 >> put i
    put (Sym s) = putI 1 >> put s
    put Asterisk = putI 2

instance Serialize MIXChar where
    get = MIXChar <$> get
    put (MIXChar c) = put c

instance Serialize DefinedSymbol where
    get = do
      c <- getI
      case c of
        0 -> DefNormal <$> get
        1 -> DefLocal <$> get
        _ -> fail "Invalid defined symbol"

    put (DefNormal s) = putI 0 >> put s
    put (DefLocal i) = putI 1 >> put i

instance Serialize DebugData where
    get = DebugData <$> get <*> get
    put (DebugData ss syms) = put ss >> put syms

instance Serialize Program where
    get = Program <$> get <*> get <*> get
    put (Program ss sa d) = put ss >> put sa >> put d
