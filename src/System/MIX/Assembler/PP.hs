module System.MIX.Assembler.PP
    ( ppBinaryWord
    , ppProgram
    )
where

import Control.Applicative ((<$>))
import Data.List (intersperse, intercalate)
import Text.PrettyPrint.HughesPJ
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

import System.MIX.Symbolic
import System.MIX.PP
import System.MIX.Assembler (Program(..))
import System.MIX.Assembler.MIXWord

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