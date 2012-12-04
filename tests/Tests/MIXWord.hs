module Tests.MIXWord
    ( tests
    )
where

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework
    ( testGroup, Test )

import System.MIX.MIXWord

tests :: Test
tests = testGroup "MIXWord tests" [
         testGroup "Byte storage tests" [
                       testIntWordConversion
                       ]
        ]

maxbits :: Int
maxbits = bitsPerByte * bytesPerWord

testIntWordConversion :: Test
testIntWordConversion =
    let ints = choose (minVal, maxVal)
        (minVal, maxVal) = (-(2^maxbits)+1, 2^maxbits-1)
    in testProperty "A test" $ forAll ints $ \i ->
        (toInt $ toWord i) == i
