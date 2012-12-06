module MIX.Assembler.PP
    ( ppBinaryWord
    , ppProgram
    )
where

import Control.Applicative ((<$>), (<|>))
import Data.List (intersperse, intercalate)
import Data.Char (intToDigit)
import Data.Maybe
import Numeric (showIntAtBase)
import Text.PrettyPrint.HughesPJ

import Language.MIXAL.AST
import Language.MIXAL.PP
import MIX.Assembler (Program(..), DebugData(..))
import MIX.Assembler.MIXWord

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

ppSymEntry :: (DefinedSymbol, MIXWord) -> Doc
ppSymEntry (sym, off) =
    ppSymbolDef sym $$
                nest 14 (
                         text " = " <> (text $ show off) $$
                         nest 20 (text " = " <> (int $ toInt off))
                        )

ppSegment :: (Int, [MIXWord]) -> Maybe [MIXALStmt] -> Doc
ppSegment (start, mixwords) mStmts =
    hang (text "Segment:") 2 $ vcat (heading : entryLines)
        where
          entries = if isJust mStmts
                    then zip mixwords (Just <$> fromJust mStmts)
                    else zip mixwords $ replicate (length mixwords) Nothing

          entryLines = ppEntry <$> zip [start..] entries
          mixalHeading = if isJust mStmts
                         then Just $ text "MIXAL"
                         else Nothing

          heading = ppLine (text "Addr") (text "MIX Word") (text "Raw") mixalHeading
          ppEntry (pc, (w, stmt)) = ppLine (int pc) (text $ show w) (text $ ppBinaryWord w) (ppMIXALStmt <$> stmt)

          ppLine c1 c2 c3 mc4 =
              let Just c4 = ((text "|" <+>) <$> mc4) <|> Just empty
              in c1 $$ (nest 6 $ text "|" <+> (c2 $$ (nest 18 $ text "|" <+> (c3 $$ (nest 37 c4)))))

ppEquivalents :: Maybe DebugData -> Doc
ppEquivalents Nothing = empty
ppEquivalents (Just debug) =
    let heading = text "Equivalents:"
    in hang heading 2 $ vcat $ ppSymEntry <$> symbols debug

ppProgram :: Program -> Doc
ppProgram p =
    let heading = text "Program start address:" <+> (int $ toInt $ startAddress p)
        mDebug = debugData p

        segDocs = segDoc <$> segments p
        segDoc (start, mixwords) =
            case mDebug of
              Nothing -> ppSegment (start, mixwords) Nothing
              Just debug -> let stmts = lookup start $ segmentStatements debug
                            in ppSegment (start, mixwords) stmts

    in vcat $ intersperse (text " ")
           [ heading
           , ppEquivalents mDebug
           , vcat $ intersperse (text " ") $ segDocs
           ]