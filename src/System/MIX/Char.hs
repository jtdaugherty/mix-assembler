module System.MIX.Char
    ( charToByte
    , mixChars
    )
where

import Data.List (elemIndex)
import System.MIX.Symbolic (MIXChar(..))
import System.MIX.MIXWord (MIXWord, toWord)

mixChars :: [Char]
mixChars = " ABCDEFGHIΔJKLMNOPQRΣΠSTUVWXYZ0123456789.,()+-*/=$<>@;:'"

charToByte :: MIXChar -> MIXWord
charToByte (MIXChar c) =
    case elemIndex c mixChars of
      Nothing -> error $ "Character not supported: " ++ [c]
      Just i -> toWord i
