module System.MIX.PP where

import Control.Applicative ((<$>))
import Data.List (intersperse, intercalate)
import Text.PrettyPrint.HughesPJ
import System.MIX.Symbolic
import System.MIX.Assembler (Program(..))
import System.MIX.MIXWord
import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Data.Maybe (isJust, fromJust)

ppBinaryWord :: MIXWord -> String
ppBinaryWord v =
    intercalate " " $ [ if isNegative v then "1" else "0"
                      , showByte b1
                      , showByte b2
                      , showByte b3
                      , showByte b4
                      , showByte b5
                      ]
        where
          showByte b = pad $ showIntAtBase 2 intToDigit b ""
          pad s = replicate (bitsPerByte - length s) '0' ++ s

          b1 = getByte 1 v
          b2 = getByte 2 v
          b3 = getByte 3 v
          b4 = getByte 4 v
          b5 = getByte 5 v

ppAddress :: Address -> Doc
ppAddress (AddrExpr e) = ppExpr e
ppAddress (AddrRef s) = ppSymbolRef s
ppAddress (AddrLiteral v) = text "=" <> ppWValue v <> text "="

mppField :: Maybe Field -> Doc
mppField Nothing = empty
mppField (Just f) = text "(" <> ppField f <> text ")"

ppWValue :: WValue -> Doc
ppWValue (WValue e f rest) =
    hcat $ addCommas $ doc <$> (e, f) : rest
        where
          doc (ex, fld) = ppExpr ex <> mppField fld
          addCommas = intersperse (text ",")

ppIndex :: Index -> Doc
ppIndex (Index i) = text $ show i

ppField :: Field -> Doc
ppField (FieldExpr e) = ppExpr e

ppBinOp :: BinOp -> Doc
ppBinOp Add = text "+"
ppBinOp Subtract = text "-"
ppBinOp Multiply = text "*"
ppBinOp Divide = text "/"
ppBinOp Frac = text "//"
ppBinOp Field = text ":"

ppOpCode :: OpCode -> Doc
ppOpCode = text . show

ppExpr :: Expr -> Doc
ppExpr (AtExpr a) = ppAtomicExpr a
ppExpr (Signed s e) = text sign <> ppAtomicExpr e
    where
      sign = if s then "-" else "+"
ppExpr (BinOp e1 rest) = hcat $ ppExpr e1 : restDocs
    where
      restDocs = pairDoc <$> rest
      pairDoc (op, e) = ppBinOp op <> ppExpr e

ppAtomicExpr :: AtomicExpr -> Doc
ppAtomicExpr (Num i) = int i
ppAtomicExpr (Sym s) = ppSymbolRef s
ppAtomicExpr Asterisk = text "*"

ppSymbolDef :: DefinedSymbol -> Doc
ppSymbolDef (DefNormal s) = ppSymbol s
ppSymbolDef (DefLocal i) = text $ (show i) ++ "H"

ppSymbolRef :: SymbolRef -> Doc
ppSymbolRef (RefNormal s) = ppSymbol s
ppSymbolRef (RefBackward i) = text $ (show i) ++ "B"
ppSymbolRef (RefForward i) = text $ (show i) ++ "F"

ppSymbol :: Symbol -> Doc
ppSymbol (Symbol s) = text s

mppSymbolDef :: Maybe DefinedSymbol -> Doc
mppSymbolDef Nothing = text " "
mppSymbolDef (Just s) = ppSymbolDef s

ppMIXALStmt :: MIXALStmt -> Doc
ppMIXALStmt (Orig s wv) =
    mppSymbolDef s $$ (nest 11 (text "ORIG" $$ (nest 5 $ ppWValue wv)))
ppMIXALStmt (Equ s wv) =
    mppSymbolDef s $$ nest 11 (text "EQU" $$ (nest 5 $ ppWValue wv))
ppMIXALStmt (Con s wv) =
    mppSymbolDef s $$ nest 11 (text "CON" $$ (nest 5 $ ppWValue wv))
ppMIXALStmt (End s wv) =
    mppSymbolDef s $$ (nest 11 (text "END" $$ (nest 5 $ ppWValue wv)))
ppMIXALStmt (Alf s (MIXChar c1, MIXChar c2, MIXChar c3, MIXChar c4, MIXChar c5)) =
    mppSymbolDef s $$ (nest 11 (text "ALF" $$ (nest 5 $ doubleQuotes (text $ c1:c2:c3:c4:c5:[]))))
ppMIXALStmt (Inst s o addr i f) =
    showSym $$ nest 11 (ppOpCode o $$ (nest 5 (ppA <> sep1 <> ppI <> ppF f)))
        where
          showSym = if isJust s
                    then ppSymbolDef $ fromJust s
                    else text " "
          sep1 = if isJust i
                 then text ","
                 else empty
          ppI = if isJust i
                then ppIndex $ fromJust i
                else empty
          ppA = if isJust addr
                then ppAddress $ fromJust addr
                else empty
          ppF Nothing = empty
          ppF (Just (FieldExpr e)) = text "(" <> ppExpr e <> text ")"

ppSymEntry :: (DefinedSymbol, MIXWord) -> Doc
ppSymEntry (sym, off) =
    ppSymbolDef sym $$
                nest 14 (
                         text " = " <> (text $ show off) $$
                         nest 20 (text " = " <> (int $ toInt off))
                        )

ppSegment :: (Int, [(MIXWord, MIXALStmt)]) -> Doc
ppSegment (start, entries) =
    hang (text "Segment:") 2 $ vcat (heading : entryLines)
        where
          entryLines = ppEntry <$> zip [start..] entries
          heading = ppLine (text "Addr") (text "MIX Word") (text "Raw") (text "MIXAL")
          ppEntry (pc, (w, stmt)) = ppLine (int pc) (text $ show w) (text $ ppBinaryWord w) (ppMIXALStmt stmt)

          ppLine c1 c2 c3 c4 =
              c1 $$ (nest 6 $ text "|" <+> (c2 $$ (nest 18 $ text "|" <+> (c3 $$ (nest 37 $ text "|" <+> c4)))))

ppProgram :: Program -> Doc
ppProgram p =
    let heading = text "Program start address:" <+> (int $ toInt $ startAddress p)
        stHeading = text "Equivalents:"
    in vcat $ intersperse (text " ")
           [ heading
           , hang stHeading 2 $ vcat $ ppSymEntry <$> symbols p
           , vcat $ intersperse (text " ") $ ppSegment <$> segments p
           ]