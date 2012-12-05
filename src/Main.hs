module Main where

import System.Environment
import Text.PrettyPrint.HughesPJ

import Language.MIXAL.Parser
import Language.MIXAL.PP (ppMIXALStmt)
import MIX.Assembler
import MIX.Assembler.PP

formatMessage :: AsmError -> String
formatMessage (UnresolvedLocalForward i Nothing) =
    "Unresolved forward reference for " ++ show i
formatMessage (UnresolvedLocalForward i (Just s)) =
    "Unresolved forward reference for " ++ show i ++ "\n  " ++ (render $ ppMIXALStmt s)
formatMessage (AsmError s Nothing) = s
formatMessage (AsmError s (Just stmt)) =
    s ++ "\nLast processed statement:\n  " ++ (render $ ppMIXALStmt stmt)

main :: IO ()
main = do
  args <- getArgs
  pName <- getProgName
  if length args /= 1 then
      error $ "Usage: " ++ pName ++ " <filename>" else
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
