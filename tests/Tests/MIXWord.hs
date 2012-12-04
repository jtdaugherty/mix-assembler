module Tests.MIXWord
    ( tests
    )
where

import qualified Data.Bits as B
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework
    ( testGroup, Test )

import System.MIX.MIXWord

tests :: Test
tests = testGroup "MIXWord tests" [
         testGroup "Byte storage tests" [
                        testIntWordConversion
                       , testTruncate
                       ]
        ]

maxbits :: Int
maxbits = bitsPerByte * bytesPerWord

mixInt :: Gen Int
mixInt = choose (minVal, maxVal)
    where
      (minVal, maxVal) = (-(2^maxbits)+1, 2^maxbits-1)

testIntWordConversion :: Test
testIntWordConversion =
    let desc = "toWord/toInt are consistent"
    in testProperty desc $ forAll mixInt $ \i ->
        and [ (toInt $ toWord i) == i
            , (toWord $ toInt $ toWord i) == toWord i
            ]

testTruncate :: Test
testTruncate =
    let desc = "Integer values are truncated past 30 bits"
    in testProperty desc $
       forAll arbitrary $ \i ->
           if i >= 0
           then (toInt $ toWord i) B..&. wordMask == (i B..&. wordMask)
           else abs (toInt (toWord i)) B..&. wordMask == abs i B..&. wordMask