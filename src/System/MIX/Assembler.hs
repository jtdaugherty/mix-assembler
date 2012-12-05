module System.MIX.Assembler
    ( LogMessage(..)
    , Program(segments, symbols, startAddress)
    , AssemblerResult(messages, program)
    , AsmError(..)
    , assemble
    )
where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe
import Data.Bits (shiftL)

import Language.MIXAL.OpCode
import qualified Language.MIXAL.AST as S
import qualified System.MIX.Assembler.MIXWord as S
import System.MIX.Assembler.Char (charToByte)

data LogMessage = Msg String (Maybe S.MIXALStmt)
                  deriving (Show)

data Intermediate = Ready S.MIXWord
                  | Unresolved S.Symbol (S.MIXWord -> S.MIXWord)

data AssemblerState =
    AS { equivalents :: [(S.DefinedSymbol, S.MIXWord)]
       -- ^Includes symbols defined with EQU as well as symbols
       -- associated with program counter values.
       , programCounter :: S.MIXWord
       , output :: [(Int, Intermediate, S.MIXALStmt)]
       , startAddr :: Maybe S.MIXWord
       , logMessages :: [LogMessage]
       , currentStatement :: S.MIXALStmt
       , litConstCounter :: Int
       , litConsts :: [(S.Symbol, S.WValue)]
       }

data Program =
    Program { segments :: [(Int, [(S.MIXWord, S.MIXALStmt)])]
            , symbols :: [(S.DefinedSymbol, S.MIXWord)]
            , startAddress :: S.MIXWord
            }

data AssemblerResult =
    AssemblerResult { messages :: [LogMessage]
                    , program :: Program
                    }

instance Error AsmError where
    noMsg = AsmError "<empty>" Nothing
    strMsg s = AsmError s Nothing

data AsmError = AsmError String (Maybe S.MIXALStmt)

type M a = ErrorT AsmError (State AssemblerState) a

curStmt :: M S.MIXALStmt
curStmt = currentStatement <$> get

setCurStmt :: S.MIXALStmt -> M ()
setCurStmt st = modify $ \s -> s { currentStatement = st }

err :: String -> M a
err s = (throwError . AsmError s) =<< (Just <$> curStmt)

initialState :: AssemblerState
initialState =
    AS { equivalents = []
       , programCounter = S.toWord 0
       , output = []
       , startAddr = Nothing
       , logMessages = []
       , currentStatement = S.Orig Nothing $ S.WValue (S.AtExpr $ S.Num 0) Nothing []
       , litConstCounter = 0
       , litConsts = []
       }

nextLitConstSym :: M S.Symbol
nextLitConstSym = do
  v <- litConstCounter <$> get
  let sname = "LITCON" ++ show v
  modify $ \s -> s { litConstCounter = v + 1 }
  return $ S.Symbol sname

getPc :: M S.MIXWord
getPc = programCounter <$> get

incPc :: M ()
incPc =
    modify (\s -> s { programCounter = S.addWord (programCounter s) (S.toWord 1) })

setPc :: S.MIXWord -> M ()
setPc i =
    modify (\s -> s { programCounter = i })

append :: Intermediate -> S.MIXALStmt -> S.MIXWord -> M ()
append inst stmt pc =
    modify (\s -> s { output = output s ++
                               [(S.toInt pc, inst, stmt)]
                    }
           )

appendLitConst :: S.Symbol -> S.WValue -> M ()
appendLitConst sym wv =
    modify (\s -> s { litConsts = litConsts s ++ [(sym, wv)]
                    }
           )

assemble :: [S.MIXALStmt] -> Either AsmError AssemblerResult
assemble ss = assembleStage1 ss >>= (return . assembleStage2)

assembleStage1 :: [S.MIXALStmt] -> Either AsmError AssemblerState
assembleStage1 ss =
    case status of
      Left e -> Left e
      Right _ -> Right st
    where
      (status, st) = runState (runErrorT (assemblyMain ss)) initialState

-- |First stage assembly.  Tracks declared symbols as it goes, using
-- them to evaluate expressions.  Converts MIXAL statements into
-- binary instructions.  Will throw an AsmError on failure.  This is
-- the first of two stages; in this stage we finish up with a
-- partially assembled program.  Some instructions will be assembled,
-- others will reference unresolved symbols assigned to literal
-- constants.
assemblyMain :: [S.MIXALStmt] -> M ()
assemblyMain ss = do
  doAssembly ss

  -- After assembling the statements from the program, we need to 1)
  -- check that an END directive was present to set the start address
  -- of the program and 2) that for each symbol reference in the
  -- program, if there isn't already a known address for the symbol,
  -- we insert a zero word labeled with the symbol in question (as per
  -- TAOCP's specification that references to symbols should
  -- effectively result in a word being reserved for them with an
  -- initial value of zero).

  curSt <- get

  when (isNothing $ startAddr curSt) $
       err "Missing END directive"

  forM_ (litConsts curSt) $ \(sym, wv) ->
      do
        st' <- get
        when (isNothing $ lookup (S.DefNormal sym) (equivalents st')) $
             assembleStatement (S.Con (Just $ S.DefNormal sym) wv)

-- |Second stage assembly.  The first stage produces a
-- partially-assembled program but leaves literal constant references
-- unresolved.  This stage of assembly happens after the user's
-- program has been scanned and assembled to the extent possible.
-- This step is responsible for processing the MIXAL statements with
-- unresolved literal constant references now that we know the
-- addresses of those literal constant words in memory.  The assembly
-- of those referencing instructions is finished by providing them
-- with the needed memory locations of the literal constant
-- expressions they referenced.  The final result is always a complete
-- program; this step can never fail.
assembleStage2 :: AssemblerState -> AssemblerResult
assembleStage2 st =
    AssemblerResult { messages = logMessages st
                    , program = p
                    }
        where
          p = Program (segs2 st) (equivalents st) (fromJust $ startAddr st)

          processIntermediate (pc, (Ready w), stmt) = (pc, w, stmt)
          processIntermediate (pc, (Unresolved s mk), stmt) =
              let Just loc = lookup (S.DefNormal s) $ equivalents st
              in (pc, mk loc, stmt)

          segs = getSegments . (processIntermediate <$>) . output
          segs2 s = extract <$> segs s
          f (_, w, s) = (w, s)
          extract [] = undefined
          extract allss@((pc, _, _):_) = (pc, f <$> allss)

getSegments :: [(Int, a, b)] -> [[(Int, a, b)]]
getSegments [] = []
getSegments es = s : rest
    where
      s = getSegment (-1) [] es
      rest = getSegments $ drop (length s) es

      -- getSegment: given a list of assembler data, return the
      -- longest prefix such that each two consecutive entries have
      -- consecutive i-values.

      -- If there's nothing left, return the current segment
      getSegment _ acc [] = acc
      -- If this is the first item we're processing, add it to the
      -- segment and keep going to seed the comparison against the
      -- i-value.
      getSegment _ [] ((i, a, b):ss) =
          getSegment i [(i, a, b)] ss
      -- If the current entry immediately follows the i-value of the
      -- previous one, add it to the current segment and continue.
      -- Otherwise, stop now and return the segment we've built.
      getSegment v acc ((i, a, b):ss) =
          if i == v + 1
          then getSegment i (acc++[(i, a, b)]) ss
          else acc

doAssembly :: [S.MIXALStmt] -> M ()
doAssembly [] = return ()
doAssembly (s:ss) = do
  setCurStmt s
  assembleStatement s
  doAssembly ss

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
evalExpr (S.BinOp e1 op1 e2 rest) = do
  e1val <- evalExpr e1
  e2val <- evalExpr e2
  let first = evalBinOp op1 e1val e2val

  let next prev (op, e) = do
        eval <- evalExpr e
        return $ evalBinOp op prev eval

  foldM next first rest

resolveSymbol :: S.SymbolRef -> M S.MIXWord
resolveSymbol (S.RefNormal s) = do
  st <- get
  let result = lookup (S.DefNormal s) $ equivalents st
  case result of
    Nothing -> err $ "No such symbol " ++ show s
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
evalAddress (S.LitConst _) = error "Should never happen"
evalAddress (S.AddrExpr e) = evalExpr e
evalAddress (S.AddrRef ref) = resolveSymbol ref
evalAddress (S.AddrLiteral l) = evalWValue l

fieldToWord :: S.Field -> M S.MIXWord
fieldToWord (S.FieldExpr e) = evalExpr e

evalWValue :: S.WValue -> M S.MIXWord
evalWValue (S.WValue e mf es) = do
  let toFieldPair f =
          case f of
            Nothing -> return (0, 5)
            Just (S.FieldExpr fe) ->
                do
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
       err ("Invalid ORIG instruction with negative argument " ++ (show v))
  setPc v
assembleStatement (S.Equ ms wv) = do
  val <- evalWValue wv
  registerSym ms val
assembleStatement s@(S.Con ms wv) = do
  w <- evalWValue wv
  pc <- getPc
  registerSym ms pc
  append (Ready w) s pc
  incPc
assembleStatement s@(S.Alf ms (c1, c2, c3, c4, c5)) = do
  let val = S.storeInField (charToByte c1) (1, 1) $
            S.storeInField (charToByte c2) (2, 2) $
            S.storeInField (charToByte c3) (3, 3) $
            S.storeInField (charToByte c4) (4, 4) $
            S.storeInField (charToByte c5) (5, 5) (S.toWord 0)
  registerSym ms =<< getPc
  append (Ready val) s =<< getPc
  incPc
assembleStatement s@(S.Inst ms op ma mi mf) = do
  pc <- getPc
  registerSym ms pc
  f <- case mf of
         Nothing -> return $ S.toWord 5 -- (0:5) = 0*8 + 5
         Just fld -> fieldToWord fld

  i <- case mi of
         Nothing -> return $ S.toWord 0
         Just (S.Index ival) -> return $ S.toWord ival

  let (opc, fld) = opCode op
  when (isJust fld && isJust mf) $
       err ("Field specification not permitted for Instruction type " ++ (show op))

  let f' = if isJust fld
           then S.toWord $ fromJust fld
           else f

      finish a =
          S.storeInField a (0, 2) $
           S.storeInField i (3, 3) $
           S.storeInField f' (4, 4) $
           S.storeInField (S.toWord opc) (5, 5) (S.toWord 0)

  case ma of
    Just (S.LitConst e) -> do
            sym <- nextLitConstSym
            let s' = S.Inst ms op (Just $ S.AddrRef $ S.RefNormal sym) mi mf
            append (Unresolved sym finish) s' pc
            appendLitConst sym e
    Nothing -> append (Ready $ finish $ S.toWord 0) s pc
    Just (S.AddrRef (S.RefNormal ref)) -> do
            append (Unresolved ref finish) s pc
            appendLitConst ref $ S.WValue (S.AtExpr $ S.Num 0) Nothing []
    Just addr -> do
            v <- evalAddress addr
            append (Ready $ finish v) s pc
  incPc
assembleStatement (S.End ms wv) = do
  registerSym ms =<< getPc
  a <- startAddr <$> get
  v <- evalWValue wv
  case a of
    Just x -> err ("Start address already declared to be " ++ show x)
    Nothing -> do
           let mx = (1 `shiftL` (2 * S.bitsPerByte)) - 1
           when (S.toInt v > mx) $
                err $ "END argument exceeds two-byte maximum (max value " ++ show mx ++ ")"
           modify $ \st -> st { startAddr = Just v }