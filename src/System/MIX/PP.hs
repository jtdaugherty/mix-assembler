module System.MIX.PP where

import Text.PrettyPrint.HughesPJ
import System.MIX.Symbolic
import Data.Maybe (isJust, fromJust)
import Data.List (intersperse)
import Control.Applicative ((<$>))

ppAddress :: Address -> Doc
ppAddress (AddrExpr e) = ppExpr e
ppAddress (AddrRef s) = ppSymbolRef s
ppAddress (AddrLiteral v) = text "=" <> ppWValue v <> text "="

ppWValue :: WValue -> Doc
ppWValue (WValue pairs) =
    hcat $ intersperse (text ",") $ showPair <$> pairs
        where
          showPair (e, f) = ppExpr e <> text "(" <> ppField f <> text ")"

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
      sign = if s then "+" else "-"
ppExpr (BinOp op a1 a2) = hcat [ ppExpr a1
                               , ppBinOp op
                               , ppAtomicExpr a2
                               ]

ppAtomicExpr :: AtomicExpr -> Doc
ppAtomicExpr (Num i) = text $ show i
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

ppDirective :: Directive -> Doc
ppDirective (ORIG i) = text "" $$ (nest 11 (text "ORIG" $$ (nest 5 $ ppExpr i)))
ppDirective (EQU s i) = ppSymbolDef s $$ nest 11 (text "EQU" $$ (nest 5 $ ppExpr i))

ppMIXALStmt :: MIXALStmt -> Doc
ppMIXALStmt (Dir d) = ppDirective d
ppMIXALStmt (Inst s o addr i f) =
    showSym $$ nest 11 (ppOpCode o $$ (nest 5 (ppA <> sep1 <> ppI <> ppF f)))
        where
          showSym = if isJust s
                    then ppSymbolDef $ fromJust s
                    else empty
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