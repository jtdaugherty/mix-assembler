module Main where

import Test.Framework ( defaultMain )

import qualified Tests.MIXWord as MIXWord

main :: IO ()
main = defaultMain [ MIXWord.tests
                   ]