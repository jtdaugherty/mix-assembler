module Main where

import System.Environment (getArgs)
import Control.Monad (forM_)
import Text.PrettyPrint.HughesPJ
import Control.Applicative

import System.MIX.MIXALParser
import System.MIX.PP
import System.MIX.Assembler

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then
      error "Usage: mixal <filename>" else
      return ()

  let [fname] = args

  s <- readFile fname
  let r = parseMIXAL fname s
  case r of
    Left e -> putStrLn $ "Error: " ++ show e
    Right is -> do
           mapM_ (putStrLn . show) is
           putStrLn ""
           putStrLn $ render $ vcat $ ppMIXALStmt <$> is

           let result = assemble is
           putStrLn "Assembler output:"

           forM_ result $ \(pc, w) ->
               putStrLn $ (show pc) ++ "     " ++ (show w)
