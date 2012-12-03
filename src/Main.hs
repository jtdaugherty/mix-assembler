module Main where

import System.Environment (getArgs)
import Text.PrettyPrint.HughesPJ

import System.MIX.MIXALParser
import System.MIX.PP
import System.MIX.Assembler

formatMessage :: AsmError -> String
formatMessage (AsmError s Nothing) = s
formatMessage (AsmError s (Just stmt)) =
    s ++ "\nLast processed statement:\n  " ++ (render $ ppMIXALStmt stmt)

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
    Right is ->
        case assemble is of
          Left e -> do
            putStrLn "Error during assembly:"
            putStrLn $ formatMessage e
          Right res -> do
            putStrLn "Assembler output:"
            putStrLn ""
            putStrLn $ render $ ppProgram $ program res
