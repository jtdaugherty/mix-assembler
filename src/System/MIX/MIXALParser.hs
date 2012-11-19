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
                              , parseInstruction
                              ]
        (char '\n' >> return ()) <|> eof
        return s

parseDirective :: Parser S.MIXALStmt
parseDirective = choice $ try <$> [ parseEqu
                                  , parseOrig
                                  , parseEnd
                                  ]

parseInstruction :: Parser S.MIXALStmt
parseInstruction = choice $ try <$> [ parseInstNoLabel
                                    , parseInstWithLabel
                                    ]

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  v <- p
  _ <- char ')'
  return v

parseInstNoLabel :: Parser S.MIXALStmt
parseInstNoLabel = do
  spaces
  op <- parseOpCode
  spaces
  a <- (Just <$> parseAddress) <|> (return Nothing)
  let parseIndex = S.Index <$> (char ',' >> parseInt)
      parseField = S.FieldExpr <$> parens parseExpr
  i <- (Just <$> parseIndex) <|> (return Nothing)
  f <- (Just <$> parseField) <|> (return Nothing)
  return $ S.Inst Nothing op a i f

parseInstWithLabel :: Parser S.MIXALStmt
parseInstWithLabel = do
  s <- parseDefinedSymbol
  spaces
  op <- parseOpCode
  spaces
  a <- (Just <$> parseAddress) <|> (return Nothing)
  let parseIndex = S.Index <$> (char ',' >> parseInt)
      parseField = S.FieldExpr <$> parens parseExpr
  i <- (Just <$> parseIndex) <|> (return Nothing)
  f <- (Just <$> parseField) <|> (return Nothing)
  return $ S.Inst (Just s) op a i f

parseAddress :: Parser S.Address
parseAddress =
    choice $ try <$> [ S.AddrRef <$> parseSymbolRef
                     , S.AddrExpr <$> parseExpr
                     , S.AddrLiteral <$> parseWValue
                     ]

parseWValue :: Parser S.WValue
parseWValue = do
  let p = do
        e <- parseExpr
        f <- S.FieldExpr <$> parens parseExpr
        return (e, f)

  S.WValue <$> sepBy1 p (char ',')

parseOpCode :: Parser S.OpCode
parseOpCode =
    choice $ try <$> (\(s, v) -> string s >> return v) <$> pairs
        where
          pairs = [ ("LDA", S.LDA), ("LDX", S.LDX), ("LD1", S.LD1)
                  , ("LD2", S.LD2), ("LD3", S.LD3), ("LD4", S.LD4)
                  , ("LD5", S.LD5), ("LD6", S.LD6), ("LDAN", S.LDAN)
                  , ("LDXN", S.LDXN), ("LD1N", S.LD1N), ("LD2N", S.LD2N)
                  , ("LD3N", S.LD3N), ("LD4N", S.LD4N), ("LD5N", S.LD5N)
                  , ("LD6N", S.LD6N), ("STA", S.STA), ("STX", S.STX)
                  , ("ST1", S.ST1), ("ST2", S.ST2), ("ST3", S.ST3)
                  , ("ST4", S.ST4), ("ST5", S.ST5), ("ST6", S.ST6)
                  , ("STJ", S.STJ), ("STZ", S.STZ), ("ADD", S.ADD)
                  , ("SUB", S.SUB), ("MUL", S.MUL), ("DIV", S.DIV)
                  , ("ENTA", S.ENTA), ("ENTX", S.ENTX), ("ENT1", S.ENT1)
                  , ("ENT2", S.ENT2), ("ENT3", S.ENT3), ("ENT4", S.ENT4)
                  , ("ENT5", S.ENT5), ("ENT6", S.ENT6), ("ENNA", S.ENNA)
                  , ("ENNX", S.ENNX), ("ENN1", S.ENN1), ("ENN2", S.ENN2)
                  , ("ENN3", S.ENN3), ("ENN4", S.ENN4), ("ENN5", S.ENN5)
                  , ("ENN6", S.ENN6), ("INCA", S.INCA), ("INCX", S.INCX)
                  , ("INC1", S.INC1), ("INC2", S.INC2), ("INC3", S.INC3)
                  , ("INC4", S.INC4), ("INC5", S.INC5), ("INC6", S.INC6)
                  , ("DECA", S.DECA), ("DECX", S.DECX), ("DEC1", S.DEC1)
                  , ("DEC2", S.DEC2), ("DEC3", S.DEC3), ("DEC4", S.DEC4)
                  , ("DEC5", S.DEC5), ("DEC6", S.DEC6), ("CMPA", S.CMPA)
                  , ("CMPX", S.CMPX), ("CMP1", S.CMP1), ("CMP2", S.CMP2)
                  , ("CMP3", S.CMP3), ("CMP4", S.CMP4), ("CMP5", S.CMP5)
                  , ("CMP6", S.CMP6), ("JMP", S.JMP), ("JSJ", S.JSJ)
                  , ("JOV", S.JOV), ("JNOV", S.JNOV), ("JL", S.JL)
                  , ("JE", S.JE), ("JG", S.JG), ("JGE", S.JGE)
                  , ("JNE", S.JNE), ("JLE", S.JLE), ("JAN", S.JAN)
                  , ("JAZ", S.JAZ), ("JAP", S.JAP), ("JANN", S.JANN)
                  , ("JANZ", S.JANZ), ("JANP", S.JANP), ("JXN", S.JXN)
                  , ("JXZ", S.JXZ), ("JXP", S.JXP), ("JXNN", S.JXNN)
                  , ("JXNZ", S.JXNZ), ("JXNP", S.JXNP), ("J1N", S.J1N)
                  , ("J1Z", S.J1Z), ("J1P", S.J1P), ("J1NN", S.J1NN)
                  , ("J1NZ", S.J1NZ), ("J1NP", S.J1NP), ("J2N", S.J2N)
                  , ("J2Z", S.J2Z), ("J2P", S.J2P), ("J2NN", S.J2NN)
                  , ("J2NZ", S.J2NZ), ("J2NP", S.J2NP), ("J3N", S.J3N)
                  , ("J3Z", S.J3Z), ("J3P", S.J3P), ("J3NN", S.J3NN)
                  , ("J3NZ", S.J3NZ), ("J3NP", S.J3NP), ("J4N", S.J4N)
                  , ("J4Z", S.J4Z), ("J4P", S.J4P), ("J4NN", S.J4NN)
                  , ("J4NZ", S.J4NZ), ("J4NP", S.J4NP), ("J5N", S.J5N)
                  , ("J5Z", S.J5Z), ("J5P", S.J5P), ("J5NN", S.J5NN)
                  , ("J5NZ", S.J5NZ), ("J5NP", S.J5NP), ("J6N", S.J6N)
                  , ("J6Z", S.J6Z), ("J6P", S.J6P), ("J6NN", S.J6NN)
                  , ("J6NZ", S.J6NZ), ("J6NP", S.J6NP), ("IN", S.IN)
                  , ("OUT", S.OUT), ("IOC", S.IOC), ("JRED", S.JRED)
                  , ("JBUS", S.JBUS), ("NUM", S.NUM), ("CHAR", S.CHAR)
                  , ("SLA", S.SLA), ("SRA", S.SRA), ("SLAX", S.SLAX)
                  , ("SRAX", S.SRAX), ("SLC", S.SLC), ("SRC", S.SRC)
                  , ("MOVE", S.MOVE), ("NOP", S.NOP), ("HLT", S.HLT)
                  ]

parseEqu :: Parser S.MIXALStmt
parseEqu = do
  s <- parseDefinedSymbol
  spaces
  _ <- string "EQU"
  spaces
  e <- parseExpr
  return $ S.Dir $ S.EQU s e

parseEnd :: Parser S.MIXALStmt
parseEnd = do
  spaces
  _ <- string "END"
  spaces
  e <- parseExpr
  return $ S.Dir $ S.END e

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
  return $ S.BinOp op e2 e1

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
           , S.Sym <$> parseSymbolRef
           , char '*' >> return S.Asterisk
           ]

parseInt :: Parser Int
parseInt = parseNeg <|> parsePos
    where
      parseNeg = char '-' >> ((-1) *) <$> parsePos
      parsePos = read <$> many1 digit

parseDefinedSymbol :: Parser S.DefinedSymbol
parseDefinedSymbol = choice $ try <$> [ parseLocalDef
                                      , S.DefNormal <$> parseSymbol
                                      ]
    where
      parseLocalDef = do
        d <- digit
        _ <- char 'H'
        return $ S.DefLocal $ read $ d:""

parseSymbolRef :: Parser S.SymbolRef
parseSymbolRef = choice $ try <$> [ parseLocalRefB
                                  , parseLocalRefF
                                  , S.RefNormal <$> parseSymbol
                                  ]
    where
      parseLocalRefB = do
        d <- digit
        _ <- char 'B'
        return $ S.RefBackward $ read $ d:""

      parseLocalRefF = do
        d <- digit
        _ <- char 'F'
        return $ S.RefForward $ read $ d:""

parseSymbol :: Parser S.Symbol
parseSymbol = do
  let symChar = oneOf ['0'..'9'] <|> oneOf ['A'..'Z']
  s <- many1 symChar
  if (length s > 10) then
      fail $ "Symbol too long: " ++ s else
      return $ S.Symbol s
