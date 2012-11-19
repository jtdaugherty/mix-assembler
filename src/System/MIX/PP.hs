module System.MIX.PP where

import Text.PrettyPrint.HughesPJ
import System.MIX.Symbolic
import Data.Maybe (isJust, fromJust)
import Data.List (intersperse)
import Control.Applicative ((<$>))

ppAddress :: Address -> Doc
ppAddress NoAddr = empty
ppAddress (AddrExpr e) = ppExpr e
ppAddress (AddrRef s) = ppSymbol s
ppAddress (AddrLiteral v) = text "=" <> ppWValue v <> text "="

ppWValue :: WValue -> Doc
ppWValue (WValue pairs) =
    hcat $ intersperse (text ",") $ showPair <$> pairs
        where
          showPair (e, f) = ppExpr e <> text "(" <> ppField f <> text ")"

ppIndex :: Index -> Doc
ppIndex (Index i) = text $ show i

ppField :: Field -> Doc
ppField Default = empty
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
ppExpr (BinOp a1 op a2) = hcat [ ppExpr a1
                               , ppBinOp op
                               , ppAtomicExpr a2
                               ]

ppAtomicExpr :: AtomicExpr -> Doc
ppAtomicExpr (Num i) = text $ show i
ppAtomicExpr (Sym s) = ppSymbol s
ppAtomicExpr Asterisk = text "*"

ppSymbol :: Symbol -> Doc
ppSymbol (Symbol s) = text s

ppDirective :: Directive -> Doc
ppDirective (ORIG i) = text "" $$ (nest 11 (text "ORIG" <+> ppExpr i))
ppDirective (EQU s i) = ppSymbol s $$ nest 11 (text "EQU" <+> ppExpr i)

ppMIXALStmt :: MIXALStmt -> Doc
ppMIXALStmt (Dir d) = ppDirective d
ppMIXALStmt (Inst s o addr i f) =
    showSym $$ nest 11 (ppOpCode o <+> ppAddress addr <>
                                 sep1 <> ppI <> ppF f)
        where
          showSym = if isJust s
                    then ppSymbol $ fromJust s
                    else empty
          sep1 = if isJust i
                 then text ","
                 else empty
          ppI = if isJust i
                then ppIndex $ fromJust i
                else empty
          ppF Nothing = empty
          ppF (Just Default) = empty
          ppF (Just (FieldExpr e)) = text "(" <> ppExpr e <> text ")"