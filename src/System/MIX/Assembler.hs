module System.MIX.Assembler where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Maybe
import qualified System.MIX.Symbolic as S
import qualified System.MIX.MIXWord as S
import System.MIX.Char (charToByte)
import System.MIX.OpCode

data AssemblerState =
    AS { equivalents :: [(S.DefinedSymbol, S.MIXWord)]
       -- ^Includes symbols defined with EQU as well as symbols
       -- associated with program counter values.
       , programCounter :: S.MIXWord
       , output :: [(Int, S.MIXWord, S.MIXALStmt)]
       , startAddr :: Maybe S.MIXWord
       }

data Program =
    Program { segments :: [(Int, [(S.MIXWord, S.MIXALStmt)])]
            , symbols :: [(S.DefinedSymbol, S.MIXWord)]
            , startAddress :: S.MIXWord
            }

type M a = State AssemblerState a

initialState :: AssemblerState
initialState = AS [] (S.toWord 0) [] Nothing

getPc :: M S.MIXWord
getPc = programCounter <$> get

incPc :: M ()
incPc =
    modify (\s -> s { programCounter = S.addWord (programCounter s) (S.toWord 1) })

setPc :: S.MIXWord -> M ()
setPc i =
    modify (\s -> s { programCounter = i })

append :: S.MIXWord -> S.MIXALStmt -> S.MIXWord -> M ()
append inst stmt pc =
    modify (\s -> s { output = output s ++
                               [(S.toInt pc, inst, stmt)]
                    }
           )

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

registerSym :: Maybe S.DefinedSymbol -> S.MIXWord -> M ()
registerSym Nothing _ = return ()
registerSym (Just sym) w =
  modify $ \s ->
      s { equivalents = equivalents s ++ [(sym, w)]
        }

evalExpr :: S.Expr -> M S.MIXWord
evalExpr (S.AtExpr ae) = evalAtomicExpr ae
evalExpr (S.Signed s ae) = do
  val <- evalAtomicExpr ae
  let sgn = if s then S.setNegative else S.clearNegative
  return $ sgn val
evalExpr (S.BinOp _ []) = error "Invalid, should be prevented by parser"
evalExpr (S.BinOp e1 rest) = do
  e1val <- evalExpr e1

  let next prev (op, e) = do
        e2val <- evalExpr e
        return $ evalBinOp op prev e2val

  foldM next e1val rest

resolveSymbol :: S.SymbolRef -> M S.MIXWord
resolveSymbol (S.RefNormal s) = do
  st <- get
  let result = lookup (S.DefNormal s) $ equivalents st
  case result of
    Nothing -> error $ "No such symbol: " ++ show s
    Just v -> return v
resolveSymbol (S.RefBackward _) = undefined
resolveSymbol (S.RefForward _) = undefined

evalAtomicExpr :: S.AtomicExpr -> M S.MIXWord
evalAtomicExpr (S.Num i) = return $ S.toWord i
evalAtomicExpr (S.Sym s) = resolveSymbol s
evalAtomicExpr S.Asterisk = programCounter <$> get

evalBinOp :: S.BinOp -> S.MIXWord -> S.MIXWord -> S.MIXWord
evalBinOp S.Add = S.addWord
evalBinOp S.Subtract = S.subWord
evalBinOp S.Multiply = S.multWord
evalBinOp S.Divide = S.divWord
evalBinOp S.Frac = undefined
evalBinOp S.Field = \a b -> S.addWord (S.multWord a (S.toWord 8)) b

evalAddress :: S.Address -> M S.MIXWord
evalAddress (S.AddrExpr e) = evalExpr e
evalAddress (S.AddrRef ref) = resolveSymbol ref
evalAddress (S.AddrLiteral l) = evalWValue l

indexToWord :: S.Index -> S.MIXWord
indexToWord (S.Index n) = S.toWord n

fieldToWord :: S.Field -> M S.MIXWord
fieldToWord (S.FieldExpr e) = evalExpr e

evalWValue :: S.WValue -> M S.MIXWord
evalWValue (S.WValue e mf es) = do
  -- XXX deal with sign bit

  let toFieldPair f =
          case f of
            Nothing -> return (0, 5)
            Just (S.FieldExpr fe) -> do
                                  v <- evalExpr fe
                                  let r = (S.toInt v) `mod` 8
                                      l = (S.toInt v) `div` 8
                                  return (l, r)

  -- Store the leftmost wvalue.
  val <- evalExpr e
  f <- toFieldPair mf

  let initial = S.storeInField val f $ S.toWord 0
      next dest (ex, fld) = do
        v <- evalExpr ex
        fv <- toFieldPair fld
        return $ S.storeInField v fv dest

  foldM next initial es

assembleStatement :: S.MIXALStmt -> M ()
assembleStatement (S.Orig ms wv) = do
  registerSym ms =<< getPc
  v <- evalWValue wv
  when (S.toInt v < 0) $
       error $ "Invalid ORIG instruction with negative argument: " ++ (show v)
  setPc v
assembleStatement (S.Equ ms wv) = do
  val <- evalWValue wv
  registerSym ms val
assembleStatement s@(S.Con ms wv) = do
  w <- evalWValue wv
  pc <- getPc
  registerSym ms pc
  append w s pc
  incPc
assembleStatement s@(S.Alf ms (c1, c2, c3, c4, c5)) = do
  let val = S.storeInField (charToByte c1) (1, 1) $
            S.storeInField (charToByte c2) (2, 2) $
            S.storeInField (charToByte c3) (3, 3) $
            S.storeInField (charToByte c4) (4, 4) $
            S.storeInField (charToByte c5) (5, 5) (S.toWord 0)
  registerSym ms =<< getPc
  -- Store the instruction.
  append val s =<< getPc
  incPc
assembleStatement s@(S.Inst ms op ma mi mf) = do
  registerSym ms =<< getPc
  f <- case mf of
         Nothing -> return $ S.toWord 5 -- (0:5) = 0*8 + 5
         Just fld -> fieldToWord fld

  i <- case mi of
         Nothing -> return $ S.toWord 0
         Just (S.Index ival) -> return $ S.toWord ival

  a <- case ma of
         Nothing -> return $ S.toWord 0
         Just addr -> evalAddress addr

  let (opc, fld) = opCode op
  when (isJust fld && isJust mf) $
       error $ "Instruction " ++ (show op) ++ " provided field specification, but this instruction does not permit one"

  let f' = if isJust fld
           then S.toWord $ fromJust fld
           else f

  let val = S.storeInField a (0, 2) $
            S.storeInField i (3, 3) $
            S.storeInField f' (4, 4) $
            S.storeInField (S.toWord opc) (5, 5) (S.toWord 0)
  append val s =<< getPc
  incPc
assembleStatement (S.End _ms wv) = do
  -- XXX deal with ms = Just.
  a <- startAddr <$> get
  v <- evalWValue wv
  case a of
    Just x -> error $ "Start address already declared to be " ++ show x
    Nothing -> modify $ \st -> st { startAddr = Just v }