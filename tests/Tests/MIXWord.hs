module Tests.MIXWord
    ( tests
    )
where

import Control.Applicative
import Data.Ix
import qualified Data.Bits as B
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework
    ( testGroup, Test )

import MIX.Assembler.MIXWord
import MIX.Assembler.PP

tests :: Test
tests = testGroup "MIXWord tests" [
         testGroup "Byte storage tests" [
                        testIntWordConversion
                       , testTruncate
                       , testStorage
                       , testStorageUntouched
                       , testMultipleStorage
                       ]
        , testGroup "Byte clearing" [
                         testClearByte
                        ]
        ]

maxbits :: Int
maxbits = bitsPerByte * bytesPerWord

storePair :: Gen (MIXWord, (Int, Int))
storePair = (,) <$> (toWord <$> mixInt) <*> arbitraryField

arbitraryField :: Gen (Int, Int)
arbitraryField = do
  l <- choose (0, 5)
  r <- choose (l, 5)
  return (l, r)

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

testStorage :: Test
testStorage =
    let desc = "Word storage operations affect the stored fields"
        idx = choose (0, 5)
    in testProperty desc $
       forAll mixInt $ \dest ->
       forAll mixInt $ \src ->
       forAll idx $ \lft ->
       forAll (idx `suchThat` (>= lft)) $ \rght ->
           -- Given source and destination words and a field
           -- specification, check that storing bytes from source into
           -- dest only affect the right fields in dest.
           let resultW = storeInField srcW (lft, rght) destW
               destW = toWord dest
               srcW = toWord src
               destRange = range (max 1 lft, rght)
               srcRange = range (max 1 lft + (bytesPerWord-rght), bytesPerWord)
               fieldPairs = zip srcRange destRange
               chkInField (srcI, destI) = getByte srcI srcW == getByte destI resultW
               msg = concat [ "Source word: " ++ show srcW ++ "\n"
                            , "Dest word: " ++ show destW ++ "\n"
                            , "Field: " ++ show (lft, rght) ++ "\n"
                            , "Source field range: " ++ show srcRange ++ "\n"
                            , "Dest field range: " ++ show destRange ++ "\n"
                            , "Result: " ++ show resultW ++ "\n"
                            ]
           in printTestCase msg $
              and [ if lft == 0 then chkInField (0, 0) else True
                  , and (chkInField <$> fieldPairs)
                  ]

testMultipleStorage :: Test
testMultipleStorage =
    let desc = "Multiple stores in one call occur from left to right"
    in testProperty desc $
       forAll (toWord <$> mixInt) $ \dest ->
       forAll (listOf storePair) $ \pairs ->
           let expected = if null pairs
                          then dest
                          else storeInField lastWord lastField prev
               (lastWord, lastField) = last pairs
               prev = storeManyInField (init pairs) dest
           in storeManyInField pairs dest == expected

testStorageUntouched :: Test
testStorageUntouched =
    let desc = "Word storage operations do not affect untouched fields"
        idx = choose (0, 5)
    in testProperty desc $
       forAll mixInt $ \dest ->
       forAll mixInt $ \src ->
       forAll idx $ \lft ->
       forAll (idx `suchThat` (>= lft)) $ \rght ->
           -- Given source and destination words and a field
           -- specification, check that storing bytes from source into
           -- dest only affect the right fields in dest.
           let resultW = storeInField srcW (lft, rght) destW
               destW = toWord dest
               srcW = toWord src
               untouchedRange = range (0, lft-1) ++ range (rght+1, bytesPerWord)
               chkInField i = getByte i destW == getByte i resultW
               msg = concat [ "Field: " ++ show (lft, rght) ++ "\n"
                            , "Source word:\n  " ++ show srcW ++ "\n"
                            , "Dest word:\n  " ++ show destW ++ "\n"
                            , "Result:\n  " ++ show resultW ++ "\n"
                            , "Untouched bytes: " ++ show untouchedRange ++ "\n"
                            ]
           in printTestCase msg $
              and (chkInField <$> untouchedRange)

testClearByte :: Test
testClearByte =
    let desc = "clearByte does the right thing"
        mask = (1 `B.shiftL` bitsPerByte) - 1
    in testProperty desc $
       forAll (choose (0, 5)) $ \i ->
           forAll mixInt $ \mi ->
               let tst = clearByte i mi == expected
                   expected = mi B..&. preserved
                   preserved = B.complement $ mask `B.shiftL` ((bytesPerWord - i) * bitsPerByte)
                   msg = concat [ "Index: " ++ show i ++ "\n"
                                , "Original:\n  " ++ show (toWord mi) ++ "\n"
                                , "  " ++ (ppBinaryWord $ toWord mi) ++ "\n"
                                , "After clearByte (" ++ (show $ clearByte i mi) ++
                                  "):\n  " ++ (show $ toWord $ clearByte i mi) ++ "\n"
                                , "  " ++ (ppBinaryWord $ toWord $ clearByte i mi) ++ "\n"
                                , "Expected:\n  " ++ (ppBinaryWord $ toWord expected) ++ "\n"
                                ]
               in printTestCase msg tst
