module System.MIX.LiteralConstant
    ( rewriteLiteralConstants
    )
where

import System.MIX.Symbolic

rewriteLiteralConstants :: [MIXALStmt] -> [MIXALStmt]
rewriteLiteralConstants = concat . map rewrite

rewrite :: MIXALStmt -> [MIXALStmt]
rewrite (Inst ms op (Just a) mf mi) = rest ++ [Inst ms op (Just newAddr) mf mi]
    where
      (newAddr, rest) = rewriteAddress a
rewrite s = [s]

rewriteAddress :: Address -> (Address, [MIXALStmt])
rewriteAddress (LitConst e) = ( AddrRef $ sr
                              , [jmp, Con (Just ds) e]
                              )
    where
      sn = "LITCONST"
      ds = DefNormal $ Symbol sn
      sr = RefNormal $ Symbol sn
      jmp = Inst Nothing JMP (Just jmpAmt) Nothing Nothing
      jmpAmt = AddrExpr $ BinOp (AtExpr Asterisk)
               [(Add, AtExpr $ Num 2)]
rewriteAddress a = (a,[])
