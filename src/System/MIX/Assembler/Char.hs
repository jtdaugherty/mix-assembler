module System.MIX.Assembler.Char
    ( charToByte
    )
where

import Data.List (elemIndex)
import System.MIX.Char (mixChars)
import System.MIX.Symbolic (MIXChar(..))

import System.MIX.Assembler.MIXWord (MIXWord, toWord)

charToByte :: MIXChar -> MIXWord
charToByte (MIXChar c) =
    case elemIndex c mixChars of
      Nothing -> error $ "Character not supported: " ++ [c]
      Just i -> toWord i
