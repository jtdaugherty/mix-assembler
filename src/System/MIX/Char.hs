module System.MIX.Char where

import Data.List (elemIndex)
import System.MIX.Symbolic (MIXChar(..))

chars :: [Char]
chars = " ABCDEFGHIΔJKLMNOPQRΣΠSTUVWXYZ0123456789.,()+-*/=$<>@;:'"

charToByte :: MIXChar -> Int
charToByte (MIXChar c) =
    case elemIndex c chars of
      Nothing -> error $ "Character not supported: " ++ [c]
      Just i -> i
