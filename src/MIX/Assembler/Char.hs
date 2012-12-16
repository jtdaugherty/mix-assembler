module MIX.Assembler.Char
    ( charToByte
    )
where

import Data.List (elemIndex)
import Language.MIXAL.Char (mixChars)
import Language.MIXAL.AST (MIXChar(..))

import MIX.Assembler.MIXWord (MIXWord, toWord)

charToByte :: MIXChar -> MIXWord
charToByte (MIXChar c) =
    case elemIndex c mixChars of
      Nothing -> error $ "Character not supported: " ++ [c]
      Just i -> toWord $ toEnum i
