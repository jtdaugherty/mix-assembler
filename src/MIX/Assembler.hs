module MIX.Assembler
    ( LogMessage(..)
    , Program(..)
    , DebugData(..)
    , AssemblerResult(messages, program)
    , AsmError(..)
    , Options(..)
    , assemble
    )
where

import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe
import Data.Bits (shiftL)
import Data.Array

import Language.MIXAL.OpCode
import qualified Language.MIXAL.AST as S
import qualified MIX.Assembler.MIXWord as S
import MIX.Assembler.Char (charToByte)

data LogMessage = Msg String (Maybe S.MIXALStmt)
                  deriving (Show)

data Intermediate = Ready S.MIXWord
                  | Unresolved S.SymbolRef (S.MIXWord -> S.MIXWord)

data AssemblerState =
    AS { equivalents :: [(S.DefinedSymbol, S.MIXWord)]
       -- ^Includes symbols defined with EQU as well as symbols
       -- associated with program counter values.
       , localSymbols :: Array Int [S.MIXWord]
       -- ^Includes only local symbols. Maps each 0-9 to a list of
       -- program counters at which that symbol was defined.
       , programCounter :: S.MIXWord
       , output :: [(Int, Intermediate, S.MIXALStmt)]
       , startAddr :: Maybe S.MIXWord
       , logMessages :: [LogMessage]
       , currentStatement :: S.MIXALStmt
       , litConstCounter :: Int
       , litConsts :: [(S.Symbol, S.WValue)]
       }

data Program =
    Program { segments :: [(Int, [S.MIXWord])]
            , startAddress :: S.MIXWord
            , debugData :: Maybe DebugData
            }

data DebugData =
    DebugData { segmentStatements :: [(Int, [S.MIXALStmt])]
              , symbols :: [(S.DefinedSymbol, S.MIXWord)]
              }

data AssemblerResult =
    AssemblerResult { messages :: [LogMessage]
                    , program :: Program
                    }

data Options =
    Options { preserveDebugData :: Bool
            }

instance Error AsmError where
    noMsg = AsmError "<empty>" Nothing
    strMsg s = AsmError s Nothing

data AsmError = AsmError String (Maybe S.MIXALStmt)
              | UnresolvedLocalForward Int (Maybe S.MIXALStmt)
              | UnresolvedSymbol S.Symbol (Maybe S.MIXALStmt)

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
       , localSymbols = array (0, 9) $ zip [0..9] $ replicate 10 []
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

assemble :: Options -> [S.MIXALStmt] -> Either AsmError AssemblerResult
assemble opts ss = assembleStage1 opts ss >>= assembleStage2 opts

assembleStage1 :: Options -> [S.MIXALStmt] -> Either AsmError AssemblerState
assembleStage1 _ ss =
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
-- expressions they referenced.
assembleStage2 :: Options -> AssemblerState -> Either AsmError AssemblerResult
assembleStage2 opts st = result
        where
          result = do
            progSegments <- segs2 st
            let p = Program (wordsOnly <$> progSegments) (fromJust $ startAddr st)
                    (dbg progSegments)
            return $ AssemblerResult { messages = logMessages st
                                     , program = p
                                     }

          dbg s = if preserveDebugData opts
                  then Just $ debug s
                  else Nothing

          debug ss = DebugData { symbols = equivalents st
                               , segmentStatements = debugSeg <$> ss
                               }

          debugSeg (pos, is) = (pos, snd <$> is)
          wordsOnly (pos, is) = (pos, fst <$> is)

          segs s = do
            final <- mapM (processIntermediate st) $ output s
            return $ getSegments final
          segs2 s = (extract <$>) <$> segs s
          f (_, w, s) = (w, s)
          extract [] = undefined
          extract allss@((pc, _, _):_) = (pc, f <$> allss)

processIntermediate :: AssemblerState
                    -> (Int, Intermediate, S.MIXALStmt)
                    -> Either AsmError (Int, S.MIXWord, S.MIXALStmt)
processIntermediate _ (pc, (Ready w), stmt) = return (pc, w, stmt)
processIntermediate st (pc, (Unresolved (S.RefNormal s) mk), stmt) =
    let loc = lookup (S.DefNormal s) $ equivalents st
    in case loc of
         Nothing -> Left (UnresolvedSymbol s $ Just stmt)
         Just v -> return (pc, mk v, stmt)
processIntermediate st (pc, (Unresolved (S.RefForward i) mk), stmt) =
    let locs = localSymbols st ! i
        possible = [a | a <- locs, S.toInt a > pc]
    in if null possible
       then Left (AsmError "no forward reference possible" $ Just stmt)
       else return (pc, mk (head possible), stmt)
processIntermediate _ (_, (Unresolved ref _), _) =
    error $ "Unexpected symbolic reference type in stage 2: " ++ show ref

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
registerSym (Just (S.DefNormal sym)) w =
    modify $ \s ->
        s { equivalents = equivalents s ++ [(S.DefNormal sym, w)]
          }
registerSym (Just (S.DefLocal i)) w =
    registerLocal i w

registerLocal :: Int -> S.MIXWord -> M ()
registerLocal num pc =
  modify $ \st ->
      let newLocals = localSymbols st // [(num, entry ++ [pc])]
          entry = localSymbols st ! num
      in st { localSymbols = newLocals
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
    Nothing -> throwError . (UnresolvedSymbol s) =<< (Just <$> curStmt)
    Just v -> return v
resolveSymbol (S.RefBackward i) = do
  pc <- getPc
  locals <- localSymbols <$> get
  let addrs = locals ! i
      before = filter (\a -> S.toInt a < S.toInt pc) addrs

  if null before then
      err ("No such local symbol " ++ show i ++ " defined previously") else
      return $ last before
resolveSymbol (S.RefForward i) = do
  pc <- getPc
  locals <- localSymbols <$> get
  let addrs = locals ! i
      after = filter (\a -> S.toInt a > S.toInt pc) addrs

  if null after then
      (throwError . UnresolvedLocalForward i) =<< (Just <$> curStmt) else
      return $ head after

evalAtomicExpr :: S.AtomicExpr -> M S.MIXWord
evalAtomicExpr (S.Num i) = return $ S.toWord i
evalAtomicExpr (S.Sym s) = resolveSymbol $ S.RefNormal s
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

evalField :: S.Field -> M S.MIXWord
evalField (S.FieldExpr e) = evalExpr e

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

  pairs <- forM ((e,mf):es) $ \(ex, fld) ->
           do
             v <- evalExpr ex
             fv <- toFieldPair fld
             return (v, fv)

  return $ S.storeManyInField pairs (S.toWord 0)

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
  let val = S.storeManyInField [ (charToByte c1, (1, 1))
                               , (charToByte c2, (2, 2))
                               , (charToByte c3, (3, 3))
                               , (charToByte c4, (4, 4))
                               , (charToByte c5, (5, 5))
                               ] (S.toWord 0)

  registerSym ms =<< getPc
  append (Ready val) s =<< getPc
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

assembleStatement s@(S.Inst ms op ma mi mf) = do
  pc <- getPc
  registerSym ms pc
  f <- case mf of
         -- (0:5) = 0*8 + 5
         Nothing -> return $ S.toWord 5
         Just fld -> evalField fld

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
          S.storeManyInField [ (a, (0, 2))
                             , (i, (3, 3))
                             , (f', (4, 4))
                             , (S.toWord opc, (5, 5))
                             ] (S.toWord 0)

  case ma of
    Just (S.LitConst e) -> do
            sym <- nextLitConstSym
            let s' = S.Inst ms op (Just $ S.AddrRef $ S.RefNormal sym) mi mf
            append (Unresolved (S.RefNormal sym) finish) s' pc
            appendLitConst sym e
    Nothing -> append (Ready $ finish $ S.toWord 0) s pc
    Just (S.AddrRef (S.RefNormal ref)) -> do
            append (Unresolved (S.RefNormal ref) finish) s pc
            appendLitConst ref $ S.WValue (S.AtExpr $ S.Num 0) Nothing []
    Just addr -> do
            let tryAddress = do
                   v <- evalAddress addr
                   append (Ready $ finish v) s pc

                -- Re-throw ordinary errors; we are looking for an
                -- unresolved local symbol error so we can defer it
                resolveLater e@(AsmError _ _) = throwError e
                resolveLater (UnresolvedSymbol sym _) =
                    append (Unresolved (S.RefNormal sym) finish) s pc
                resolveLater (UnresolvedLocalForward num _) =
                    append (Unresolved (S.RefForward num) finish) s pc

            tryAddress `catchError` resolveLater
  incPc
