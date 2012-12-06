module Main where

import Control.Applicative
import Control.Monad
import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Maybe
import Text.PrettyPrint.HughesPJ

import Language.MIXAL.Parser
import Language.MIXAL.PP (ppMIXALStmt)
import MIX.Assembler
import MIX.Assembler.PP
import MIX.Assembler.IO

data Flag = DoDebug
          | Output FilePath
          | PrettyPrint
          | Help
            deriving (Eq)

flags :: [OptDescr Flag]
flags = [ Option "d" ["debug"] (NoArg DoDebug)
          "Generate debugging information in the output binary"
        , Option "o" ["output"] (ReqArg Output "FILENAME")
          "Where to write the assembled binary"
        , Option "p" ["pretty-print"] (NoArg PrettyPrint)
          "Pretty-print the resulting program"
        , Option "h" ["help"] (NoArg Help)
          "This help output"
        ]

formatMessage :: AsmError -> String
formatMessage (UnresolvedLocalForward i Nothing) =
    "Unresolved forward reference for " ++ show i
formatMessage (UnresolvedLocalForward i (Just s)) =
    "Unresolved forward reference for " ++ show i ++ "\n  " ++ (render $ ppMIXALStmt s)
formatMessage (UnresolvedSymbol sym (Just s)) =
    "Unresolved symbol " ++ show sym ++ "\n  " ++ (render $ ppMIXALStmt s)
formatMessage (UnresolvedSymbol sym Nothing) =
    "Unresolved symbol " ++ show sym
formatMessage (AsmError s Nothing) = s
formatMessage (AsmError s (Just stmt)) =
    s ++ "\nLast processed statement:\n  " ++ (render $ ppMIXALStmt stmt)

getOutputFile :: [Flag] -> Maybe FilePath
getOutputFile fs = listToMaybe $ catMaybes $
                   outputFile <$> fs
    where
      outputFile (Output f) = Just f
      outputFile _ = Nothing

usage :: IO ()
usage = do
  pName <- getProgName
  let header = "Usage: " ++ pName ++ " <path> [options]"
  putStr $ usageInfo header flags

main :: IO ()
main = do
  (flgs, args, rest) <- getOpt Permute flags <$> getArgs

  when (Help `elem` flgs) $ do
         usage
         exitFailure

  when (not $ null rest) $ do
         mapM_ putStrLn rest
         usage
         exitFailure

  when (length args /= 1) $ do
         putStrLn "No input program specified"
         usage
         exitFailure

  let [fname] = args
      opts = Options { preserveDebugData = DoDebug `elem` flgs
                     }

  s <- readFile fname
  let r = parseMIXAL fname s
  case r of
    Left e -> putStrLn $ "Error: " ++ show e
    Right is ->
        case assemble opts is of
          Left e -> do
            putStrLn "Error during assembly:"
            putStrLn $ formatMessage e
          Right res -> do
            putStrLn "Assembly successful."
            when (PrettyPrint `elem` flgs) $
                 putStrLn $ render $ ppProgram $ (program res)
            case getOutputFile flgs of
              Nothing -> putStrLn "Output not written."
              Just f -> do
                         h <- openFile f WriteMode
                         writeProgram h $ program res
                         hClose h
                         putStrLn $ "Output written to " ++ show f
