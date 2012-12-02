module Main where

import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJ
import Control.Applicative

import System.MIX.MIXALParser
import System.MIX.PP
import System.MIX.Assembler
import System.MIX.LiteralConstant

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

           putStrLn "Rewritten results:"
           mapM_ (putStrLn . show) $ rewriteLiteralConstants is

           putStrLn "Rewritten results (pretty):"
           putStrLn ""
           putStrLn $ render $ vcat $ ppMIXALStmt <$> rewriteLiteralConstants is

           let program = assemble is
           putStrLn "Assembler output:"
           putStrLn ""
           putStrLn $ render $ ppProgram program
