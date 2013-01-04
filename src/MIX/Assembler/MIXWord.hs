module MIX.Assembler.MIXWord
    ( MIXWord
    , wordMask
    , getByte
    , storeInField
    , storeManyInField
    , toWord
    , wordToInteger
    , setNegative
    , clearNegative
    , clearByte

    , addWord
    , subWord
    , multWord
    , divWord

    , isNegative
    , bitsPerByte
    , bytesPerWord
    )
where

import Control.Applicative ((<$>))
import Data.Bits
import Data.List (intersperse)
import Numeric (showHex)

newtype MIXWord = MW Integer
    deriving (Eq)

instance Show MIXWord where
    show w = concat $ intersperse " " (sign:bytes)
        where
          sign = if isNegative w then "-" else "+"
          bytes = [ showByte (getByte 1 w)
                  , showByte (getByte 2 w)
                  , showByte (getByte 3 w)
                  , showByte (getByte 4 w)
                  , showByte (getByte 5 w)
                  ]

showByte :: Integer -> String
showByte b = pad ++ h
    where
      h = showHex b ""
      pad = if length h == 1 then "0" else ""

wordMask :: Integer
wordMask = 2 ^ (bitsPerByte * bytesPerWord) - 1

toWord :: Integer -> MIXWord
toWord i =
    MW $ if i < 0
         then (abs i .&. wordMask) .|. signBit
         else abs i .&. wordMask

wordToInteger :: MIXWord -> Integer
wordToInteger (MW v) =
    let sgn = if v .&. signBit == signBit
              then (-1)
              else 1
    in (v .&. (complement signBit)) * sgn

isNegative :: MIXWord -> Bool
isNegative (MW v) = v .&. signBit == signBit

setNegative :: MIXWord -> MIXWord
setNegative (MW v) = MW $ v .|. signBit

clearNegative :: MIXWord -> MIXWord
clearNegative (MW v) = MW $ v .&. (complement signBit)

signBit :: Integer
signBit = 0x1 `shiftL` (fromEnum $ bitsPerByte * bytesPerWord)

byteMask :: Integer
byteMask = (1 `shiftL` (fromEnum bitsPerByte)) - 1

getByte :: Integer -> MIXWord -> Integer
getByte num (MW i) =
    -- Right-shift the byte of interest down to the first 6 bits and
    -- then mask it
    shiftR i (fromEnum $ (bytesPerWord - num) * bitsPerByte) .&. byteMask

bitsPerByte :: Integer
bitsPerByte = 6

bytesPerWord :: Integer
bytesPerWord = 5

storeManyInField :: [(MIXWord, (Integer, Integer))] -> MIXWord -> MIXWord
storeManyInField many base =
    foldl (flip $ uncurry $ storeInField) base many

storeInField :: MIXWord -> (Integer, Integer) -> MIXWord -> MIXWord
storeInField (MW sv) (left, right) (MW d) =
    let shiftAmt = (bytesPerWord - right) * bitsPerByte
        totalBits = (right - left + 1) * bitsPerByte
        keepBits = (1 `shiftL` (fromEnum totalBits)) - 1
        sval = (sv .&. keepBits) `shiftL` (fromEnum shiftAmt)
        left' = max 1 left
        final = sval .|. (clearBytes [left'..right] d)
        sgn = if left == 0
              then sv .&. signBit
              else d .&. signBit
        finalWithSign = sgn .|. clearByte 0 final
    in MW finalWithSign

clearBytes :: (Bits a) => [Integer] -> a -> a
clearBytes [] = id
clearBytes (i:is) = clearByte i . clearBytes is

-- byte 0: bits 30-31
-- byte 1: bits 24-29
-- byte 2: bits 18-23
-- byte 3: bits 12-17
-- byte 4: bits 6-11
-- byte 5: bits 0-5
clearByte :: (Bits a) => Integer -> a -> a
clearByte i val =
    let base = (bytesPerWord - i) * bitsPerByte
    in foldr (flip clearBit) val (fromEnum <$> [base..base+bitsPerByte-1])

addWord :: MIXWord -> MIXWord -> MIXWord
addWord a b = toWord $ wordToInteger a + wordToInteger b

subWord :: MIXWord -> MIXWord -> MIXWord
subWord a b = toWord $ wordToInteger a - wordToInteger b

divWord :: MIXWord -> MIXWord -> MIXWord
divWord a b = toWord $ div (wordToInteger a) (wordToInteger b)

multWord :: MIXWord -> MIXWord -> MIXWord
multWord a b = toWord $ (wordToInteger a) * (wordToInteger b)