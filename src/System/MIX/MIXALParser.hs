module System.MIX.MIXALParser
    ( parseMIXAL
    )
where

import qualified System.MIX.Symbolic as S
import System.MIX.Char (chars)
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Text.ParserCombinators.Parsec

parseMIXAL :: String -> String -> Either ParseError [S.MIXALStmt]
parseMIXAL filename doc = parse mixalParser filename doc

mixalParser :: Parser [S.MIXALStmt]
mixalParser = do
  ss <- many1 p
  eof
  return ss
    where
      p = do
        s <- parseStmt
        (many1 (char '\n') >> return ()) <|> eof
        return s

parseStmt :: Parser S.MIXALStmt
parseStmt = choice (try <$> choices)
    where
      choices = concat [ withoutLabel <$> stmts
                       , withLabel <$> stmts
                       ]

      withoutLabel p = spaces >> p Nothing

      withLabel p = do
        s <- parseDefinedSymbol
        _ <- many1 space
        p $ Just s

      stmts = [ parseEqu
              , parseOrig
              , parseEnd
              , parseCon
              , parseAlf
              , parseInst
              ]

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  v <- p
  _ <- char ')'
  return v

parseAddress :: Parser S.Address
parseAddress =
    choice $ try <$> [ S.LitConst <$> parseLitConst
                     , S.AddrRef <$> parseSymbolRef
                     , S.AddrExpr <$> parseExpr
                     , S.AddrLiteral <$> parseWValue
                     ]

parseWValue :: Parser S.WValue
parseWValue = do
  let p = do
        e <- parseExpr
        f <- choice [ Just <$> S.FieldExpr <$> (try $ parens parseExpr)
                    , return Nothing
                    ]
        return (e, f)

      pairs = sepBy1 p (char ',')
      mkWValue [] = error "This case should be impossible due to sepBy1 failing"
      mkWValue ((e, f):ps) = S.WValue e f ps

  mkWValue <$> pairs

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

-- These parsers are intended to be combined with a parser that will
-- try to parse them, or, failing that, parse first a defined symbol,
-- spaces, and then this parser.
parseInst :: Maybe S.DefinedSymbol -> Parser S.MIXALStmt
parseInst s = do
  op <- parseOpCode
  _ <- many1 space
  a <- (Just <$> parseAddress) <|> (return Nothing)
  let parseIndex = S.Index <$> (char ',' >> parseInt)
      parseField = S.FieldExpr <$> parens parseExpr
  i <- (Just <$> parseIndex) <|> (return Nothing)
  f <- (Just <$> parseField) <|> (return Nothing)
  return $ S.Inst s op a i f

parseEqu :: Maybe S.DefinedSymbol -> Parser S.MIXALStmt
parseEqu s = do
  _ <- string "EQU"
  _ <- many1 space
  w <- parseWValue
  return $ S.Equ s w

parseEnd :: Maybe S.DefinedSymbol -> Parser S.MIXALStmt
parseEnd s = do
  _ <- string "END"
  _ <- many1 space
  w <- parseWValue
  return $ S.End s w

parseOrig :: Maybe S.DefinedSymbol -> Parser S.MIXALStmt
parseOrig s = do
  _ <- string "ORIG"
  _ <- many1 space
  w <- parseWValue
  return $ S.Orig s w

parseCon :: Maybe S.DefinedSymbol -> Parser S.MIXALStmt
parseCon s = do
  _ <- string "CON"
  _ <- many1 space
  w <- parseWValue
  return $ S.Con s w

mixChar :: Parser S.MIXChar
mixChar = S.MIXChar <$> oneOf chars

parseAlf :: Maybe S.DefinedSymbol -> Parser S.MIXALStmt
parseAlf s = do
  _ <- string "ALF"
  _ <- many1 space
  -- XXX MIXAL doesn't use quotes but we use them to parse the chars
  -- in ALF because we don't enforce the number of spaces between the
  -- OP and the ADDRESS components of a line.
  _ <- char '"'
  chs <- replicateM 5 mixChar
  _ <- char '"'
  let cs = ( chs !! 0
           , chs !! 1
           , chs !! 2
           , chs !! 3
           , chs !! 4
           )
  return $ S.Alf s cs

parseExpr :: Parser S.Expr
parseExpr =
    -- Note: BinOps must come first to encourage the parser to try
    -- parsing a maximal expression first.  If we try atomic or signed
    -- expressions first, we'll only parse the first token in an
    -- expression and leave the rest to confuse subsequent parsers.
    choice $ try <$> [ parseBinOpExpr
                     , parseSignedExpr
                     , S.AtExpr <$> parseAtomicExpr
                     ]

parseLitConst :: Parser S.WValue
parseLitConst = do
  _ <- char '='
  e <- parseWValue
  _ <- char '='
  return e

parseBinOpExpr :: Parser S.Expr
parseBinOpExpr = do
  e1 <- choice [ S.AtExpr <$> parseAtomicExpr
               , parseSignedExpr
               ]
  rest <- many1 $ do
            op <- parseBinOp
            e <- choice [ S.AtExpr <$> parseAtomicExpr
                        , parseSignedExpr
                        ]
            return (op, e)

  return $ S.BinOp e1 rest

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
  sign <- (char '+' >> return False) <|>
          (char '-' >> return True)
  e <- parseAtomicExpr
  return $ S.Signed sign e

parseAtomicExpr :: Parser S.AtomicExpr
parseAtomicExpr =
    choice $ try <$> [ S.Num <$> parseInt
                     , S.Sym <$> parseSymbolRef
                     , char '*' >> return S.Asterisk
                     ]

parseInt :: Parser Int
parseInt = read <$> many1 digit

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
  let startChar = oneOf ['A'..'Z']
      restChar = oneOf ['0'..'9'] <|> oneOf ['A'..'Z']
  c <- startChar
  s <- many restChar
  if (length s > 9) then
      fail $ "Symbol too long: " ++ (c:s) else
      return $ S.Symbol (c:s)
