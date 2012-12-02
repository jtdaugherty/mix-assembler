module System.MIX.LiteralConstant
    ( rewriteLiteralConstants
    )
where

import System.MIX.Symbolic

rewriteLiteralConstants :: [MIXALStmt] -> [MIXALStmt]
rewriteLiteralConstants = concatMap rewrite

rewrite :: MIXALStmt -> [MIXALStmt]
rewrite (Inst ms op (Just a) mf mi) =
    rest ++ [Inst ms op (Just newAddr) mf mi]
    where
      (newAddr, rest) = rewriteAddress a
rewrite s = [s]

rewriteAddress :: Address -> (Address, [MIXALStmt])
rewriteAddress (LitConst e) = ( AddrExpr ref
                              , [jmp, Con Nothing e]
                              )
    where
      jmp = Inst Nothing JMP (Just jmpAmt) Nothing Nothing
      jmpAmt = AddrExpr $ BinOp (AtExpr Asterisk)
               [(Add, AtExpr $ Num 2)]
      ref = BinOp (AtExpr Asterisk) [(Subtract, AtExpr $ Num 1)]
rewriteAddress a = (a,[])
