module System.MIX.Assembler
    ( assemble
    , Program(startAddress, segments, symbols)
    , MIXWord(..)
    )
where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Bits
import Data.Char (intToDigit)
import Data.Maybe
import Numeric (showIntAtBase, showHex)
import qualified System.MIX.Symbolic as S
import System.MIX.Char (charToByte)
import System.MIX.OpCode

data AssemblerState =
    AS { equivalents :: [(S.DefinedSymbol, Int)]
       -- ^Includes symbols defined with EQU as well as symbols
       -- associated with program counter values.
       , programCounter :: Int
       , output :: [(Int, MIXWord, S.MIXALStmt)]
       , startAddr :: Maybe Int
       }

data Program =
    Program { segments :: [(Int, [(MIXWord, S.MIXALStmt)])]
            , symbols :: [(S.DefinedSymbol, Int)]
            , startAddress :: Int
            }

type M a = State AssemblerState a

data MIXWord = MW Bool Int
               deriving (Eq)

instance Show MIXWord where
    show (MW s v) = concat [ sgn
                           , " "
                           , showHex (getByte 1 v) " "
                           , showHex (getByte 2 v) " "
                           , showHex (getByte 3 v) " "
                           , showHex (getByte 4 v) " "
                           , showHex (getByte 5 v) ""
                           ]
        where
          sgn = if s then "-" else "+"

showBinary :: Int -> String
showBinary v = showIntAtBase 2 intToDigit (abs v) ""

byteMask :: Int
byteMask = 0x3F -- 111111

getByte :: Int -> Int -> Int
getByte num i =
    -- Right-shift the byte of interest down to the first 6 bits and
    -- then mask it
    shiftR i ((bytesPerWord - num) * bitsPerByte) .&. byteMask
bitsPerByte :: Int
bitsPerByte = 6

bytesPerWord :: Int
bytesPerWord = 5

initialState :: AssemblerState
initialState = AS [] 0 [] Nothing

getPc :: M Int
getPc = programCounter <$> get

incPc :: M ()
incPc =
    modify (\s -> s { programCounter = programCounter s + 1 })

setPc :: Int -> M ()
setPc i =
    modify (\s -> s { programCounter = i })

append :: MIXWord -> S.MIXALStmt -> Int -> M ()
append inst stmt pc =
    modify (\s -> s { output = output s ++ [(pc, inst, stmt)] })

assemble :: [S.MIXALStmt] -> Program
assemble ss =
    if isNothing $ startAddr st
    then error "Missing END in program"
    else Program segs2 (equivalents st) (fromJust $ startAddr st)
        where
          st = execState (doAssembly ss) initialState
          segs = getSegments $ output st
          segs2 = extract <$> segs
          f (_, w, s) = (w, s)
          extract [] = undefined
          extract allss@((pc, _, _):_) = (pc, f <$> allss)

getSegments :: [(Int, a, b)] -> [[(Int, a, b)]]
getSegments [] = []
getSegments es = s : rest
    where
      s = getSegment (-1) [] es
      rest = getSegments $ drop (length s) es

      getSegment _ acc [] = acc
      getSegment v acc ((i, a, b):ss) =
          if null acc
          then getSegment i [(i, a, b)] ss
          else if i == v + 1
               then getSegment i (acc++[(i, a, b)]) ss
               else acc

doAssembly :: [S.MIXALStmt] -> M ()
doAssembly [] = return ()
doAssembly (s:ss) = assembleStatement s >> doAssembly ss

registerSym :: Maybe S.DefinedSymbol -> Int -> M ()
registerSym Nothing _ = return ()
registerSym (Just sym) i =
  modify $ \s ->
      s { equivalents = equivalents s ++ [(sym, i)]
        }

evalExpr :: S.Expr -> M Int
evalExpr (S.AtExpr ae) = evalAtomicExpr ae
evalExpr (S.Signed s ae) = do
  val <- evalAtomicExpr ae
  let sVal = if s then 1 else -1
  return $ val * sVal
evalExpr (S.BinOp _ []) = error "Invalid, should be prevented by parser"
evalExpr (S.BinOp e1 rest) = do
  e1val <- evalExpr e1

  let next prev (op, e) = do
        e2val <- evalExpr e
        return $ evalBinOp op prev e2val

  foldM next e1val rest

resolveSymbol :: S.SymbolRef -> M Int
resolveSymbol (S.RefNormal s) = do
  st <- get
  let result = lookup (S.DefNormal s) $ equivalents st
  case result of
    Nothing -> error $ "No such symbol: " ++ show s
    Just v -> return v
resolveSymbol (S.RefBackward _) = undefined
resolveSymbol (S.RefForward _) = undefined

evalAtomicExpr :: S.AtomicExpr -> M Int
evalAtomicExpr (S.Num i) = return i
evalAtomicExpr (S.Sym s) = resolveSymbol s
evalAtomicExpr S.Asterisk = programCounter <$> get

evalBinOp :: S.BinOp -> Int -> Int -> Int
evalBinOp S.Add = (+)
evalBinOp S.Subtract = (-)
evalBinOp S.Multiply = (*)
evalBinOp S.Divide = div
evalBinOp S.Frac = undefined
evalBinOp S.Field = \a b -> a * 8 + b

evalAddress :: S.Address -> M Int
evalAddress (S.AddrExpr e) = evalExpr e
evalAddress (S.AddrRef ref) = resolveSymbol ref
evalAddress (S.AddrLiteral l) = evalWValue l

storeInField' :: Maybe S.Address -> (Int, Int) -> Int -> M Int
storeInField' Nothing _ v = return v
storeInField' (Just a) (left, right) d = do
  s <- evalAddress a

  let shiftAmt = (bytesPerWord - right) * bitsPerByte
      sval = shiftL s shiftAmt
      final = sval .|. (clearBytes [left..right] d)

  return final

storeInField :: Int -> Maybe S.Field -> Int -> M Int
storeInField s Nothing _ = return s
storeInField s (Just (S.FieldExpr f)) d = do
  fval <- evalExpr f
  let right = fval `mod` 8
      left = (fval - right) `div` 8
  storeInField' (Just $ toAddr s) (left, right) d

toAddr :: Int -> S.Address
toAddr = S.AddrExpr . S.AtExpr . S.Num

indexToAddr :: S.Index -> S.Address
indexToAddr (S.Index n) = toAddr n

fieldToAddr :: S.Field -> M S.Address
fieldToAddr (S.FieldExpr e) = toAddr <$> evalExpr e

clearBytes :: (Bits a) => [Int] -> a -> a
clearBytes [] = id
clearBytes (i:is) = clearByte i . clearBytes is

-- byte 0: bits 30-31
-- byte 1: bits 24-29
-- byte 2: bits 18-23
-- byte 3: bits 12-17
-- byte 4: bits 6-11
-- byte 5: bits 0-5
clearByte :: (Bits a) => Int -> a -> a
clearByte i val =
    let base = (bytesPerWord - i) * bitsPerByte
    in foldr (flip clearBit) val [base..base+bitsPerByte-1]

evalWValue :: S.WValue -> M Int
evalWValue (S.WValue e mf rest) = do
  -- XXX deal with sign bit

  -- Store the leftmost wvalue.
  val <- evalExpr e
  initial <- storeInField val mf 0

  let next dest (ex, f) = do
        v <- evalExpr ex
        storeInField v f dest

  foldM next initial rest

assembleStatement :: S.MIXALStmt -> M ()
assembleStatement (S.Orig ms wv) = do
  registerSym ms =<< getPc
  evalWValue wv >>= setPc
assembleStatement (S.Equ ms wv) = do
  val <- evalWValue wv
  registerSym ms val
assembleStatement (S.Con ms wv) = do
  val <- evalWValue wv
  pc <- getPc
  -- XXX storeStmt; need types to represent statements after
  -- processing (i.e. abstract assembled statements)
  registerSym ms pc
  incPc
assembleStatement s@(S.Alf ms (c1, c2, c3, c4, c5)) = do
  val <- storeInField' (Just $ toAddr $ charToByte c1) (1, 1) =<<
         storeInField' (Just $ toAddr $ charToByte c2) (2, 2) =<<
         storeInField' (Just $ toAddr $ charToByte c3) (3, 3) =<<
         storeInField' (Just $ toAddr $ charToByte c4) (4, 4) =<<
         storeInField' (Just $ toAddr $ charToByte c5) (5, 5) 0
  registerSym ms =<< getPc
  -- Store the instruction.
  append (MW False val) s =<< getPc
  incPc
assembleStatement s@(S.Inst ms op ma mi mf) = do
  registerSym ms =<< getPc
  f <- case mf of
         Nothing -> return Nothing
         Just fld -> Just <$> fieldToAddr fld

  val <- storeInField' ma (1, 2) =<<
         storeInField' (indexToAddr <$> mi) (3, 3) =<<
         storeInField' f (4, 4) =<<
         storeInField' (Just $ toAddr $ opCode op) (5, 5) 0
  append (MW False val) s =<< getPc
  incPc
assembleStatement s@(S.End ms wv) = do
  -- XXX deal with ms = Just.
  a <- startAddr <$> get
  v <- evalWValue wv
  case a of
    Just x -> error $ "Start address already declared to be " ++ show x
    Nothing -> modify $ \st -> st { startAddr = Just v }