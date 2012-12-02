module System.MIX.LiteralConstant
    ( rewriteLiteralConstants
    )
where

import Control.Applicative
import System.MIX.Symbolic

rewriteLiteralConstants :: [MIXALStmt] -> [MIXALStmt]
rewriteLiteralConstants [] = []
rewriteLiteralConstants (s:ss) =
    result ++ (rewrittenS : rewriteLiteralConstants ss)
        where
          (rewrittenS, rewriteResult) = rewrite s
          result = if null rewriteResult
                   then []
                   else jmp : rewriteResult
          jmp = Inst Nothing JMP (Just jmpAmt) Nothing Nothing
          jmpAmt = AddrExpr $ BinOp (AtExpr Asterisk)
                   [(Add, AtExpr $ Num $ length result)]

class Rewritable a where
    rewrite :: a -> (a, [MIXALStmt])

instance Rewritable MIXALStmt where
    rewrite (Orig ms wv) = (Orig ms new, result)
        where
          (new, result) = rewrite wv
    rewrite (Equ ms wv) = (Equ ms new, result)
        where
          (new, result) = rewrite wv
    rewrite (Con ms wv) = (Con ms new, result)
        where
          (new, result) = rewrite wv
    rewrite (Inst ms op ma mi mf) = (Inst ms op newA mi newF, result)
        where
          result = concat [ resultA
                          , resultF
                          ]
          (newA, resultA) = rewrite ma
          (newF, resultF) = rewrite mf
    rewrite (End ms wv) = (End ms new, result)
        where
          (new, result) = rewrite wv
    rewrite s = (s, [])

instance Rewritable a => Rewritable (Maybe a) where
    rewrite Nothing = (Nothing, [])
    rewrite (Just a) = (Just a', result)
        where
          (a', result) = rewrite a

instance (Rewritable a, Rewritable b) => Rewritable (a, b) where
    rewrite (a, b) = ((newA, newB), resultA ++ resultB)
        where
          (newA, resultA) = rewrite a
          (newB, resultB) = rewrite b

instance (Rewritable a, Rewritable b, Rewritable c) => Rewritable (a, b, c) where
    rewrite (a, b, c) = ((newA, newB, newC), resultA ++ resultB ++ resultC)
        where
          (newA, resultA) = rewrite a
          (newB, resultB) = rewrite b
          (newC, resultC) = rewrite c

instance Rewritable a => Rewritable [a] where
    rewrite [] = ([], [])
    rewrite (a:as) = (newA : restAs, resultA ++ resultRest)
        where
          (newA, resultA) = rewrite a
          (restAs, resultRest) = rewrite as

instance Rewritable WValue where
    rewrite (WValue e mf rest) = (WValue newE newF newRest, result)
        where
          ((newE, newF, newRest), result) = rewrite (e, mf, rest)

instance Rewritable Address where
    rewrite (AddrExpr e) = (AddrExpr e', result)
        where
          (e', result) = rewrite e
    rewrite (AddrLiteral wv) = (AddrLiteral wv', result)
        where
          (wv', result) = rewrite wv
    rewrite a = (a, [])

instance Rewritable Expr where
    rewrite (AtExpr ae) = (AtExpr ae', result)
        where
          (ae', result) = rewrite ae
    rewrite (Signed b e) = (Signed b e', result)
        where
          (e', result) = rewrite e
    rewrite (BinOp e rest) = (BinOp e' rest', resultE ++ resultRest)
        where
          (e', resultE) = rewrite e
          rest' = zip ops newEs
          es = snd <$> rest
          ops = fst <$> rest
          (newEs, resultRest) = rewrite es
    rewrite (LitConst e) = (AtExpr (Sym sr),
                                       [Con (Just ds) $
                                            WValue e Nothing []])
        where
          sn = "blah"
          ds = DefNormal $ Symbol sn
          sr = RefNormal $ Symbol sn

instance Rewritable Field where
    rewrite (FieldExpr e) = (FieldExpr e', result)
        where
          (e', result) = rewrite e

instance Rewritable AtomicExpr where
    rewrite s = (s, [])
