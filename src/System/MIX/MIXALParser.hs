module System.MIX.MIXALParser
    ( parseMIXAL
    )
where

import qualified System.MIX.Symbolic as S
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec

parseMIXAL :: String -> String -> Either ParseError [S.MIXALStmt]
parseMIXAL filename doc = parse mixalParser filename doc

mixalParser :: Parser [S.MIXALStmt]
mixalParser = many1 p
    where
      p = do
        s <- choice $ try <$> [ parseDirective
                              ]
        _ <- try $ char '\n'
        return s

parseDirective :: Parser S.MIXALStmt
parseDirective = choice $ try <$> [ parseEqu
                                  , parseOrig
                                  ]

parseEqu :: Parser S.MIXALStmt
parseEqu = do
  s <- parseSymbol
  spaces
  _ <- string "EQU"
  spaces
  e <- parseExpr
  return $ S.Dir $ S.EQU s e

parseOrig :: Parser S.MIXALStmt
parseOrig = do
  spaces
  _ <- string "ORIG"
  spaces
  e <- parseExpr
  return $ S.Dir $ S.ORIG e

parseExpr :: Parser S.Expr
parseExpr =
    choice $ try <$> [ parseBinOpExpr
                     , S.AtExpr <$> parseAtomicExpr
                     , parseSignedExpr
                     ]

parseBinOpExpr :: Parser S.Expr
parseBinOpExpr = do
  e1 <- parseAtomicExpr
  op <- parseBinOp
  e2 <- parseExpr
  return $ S.BinOp e2 op e1

parseBinOp :: Parser S.BinOp
parseBinOp =
    choice [ char '+' >> return S.Add
           , char '-' >> return S.Subtract
           , char '*' >> return S.Multiply
           , string "//" >> return S.Frac
           , char '/' >> return S.Divide
           , char ':' >> return S.Field
           ]

parseSignedExpr :: Parser S.Expr
parseSignedExpr = do
  sign <- (char '+' >> return True) <|>
          (char '-' >> return False)
  e <- parseAtomicExpr
  return $ S.Signed sign e

parseAtomicExpr :: Parser S.AtomicExpr
parseAtomicExpr =
    choice [ S.Num <$> parseInt
           , S.Sym <$> parseSymbol
           , char '*' >> return S.Asterisk
           ]

parseInt :: Parser Int
parseInt = parseNeg <|> parsePos
    where
      parseNeg = char '-' >> ((-1) *) <$> parsePos
      parsePos = read <$> many1 digit

parseSymbol :: Parser S.Symbol
parseSymbol = do
  let symChar = oneOf ['0'..'9'] <|> oneOf ['A'..'Z']
  s <- many1 symChar
  if (length s > 10) then
      fail $ "Symbol too long: " ++ s else
      return $ S.Symbol s
