module System.MIX.Char where

import Data.List (elemIndex)
import System.MIX.Symbolic (MIXChar(..))
import System.MIX.MIXWord (MIXWord, toWord)

chars :: [Char]
chars = " ABCDEFGHIΔJKLMNOPQRΣΠSTUVWXYZ0123456789.,()+-*/=$<>@;:'"

charToByte :: MIXChar -> MIXWord
charToByte (MIXChar c) =
    case elemIndex c chars of
      Nothing -> error $ "Character not supported: " ++ [c]
      Just i -> toWord i
