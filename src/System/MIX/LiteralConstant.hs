module System.MIX.LiteralConstant
    ( rewriteLiteralConstants
    )
where

import System.MIX.Symbolic

-- Rewrite a MIXAL program so that literal constants are moved into
-- memory cells, with the instructions that referenced them updated to
-- use PC-relative addresses.  This works by inserting the constant
-- instruction just prior to the place it is referenced, and also
-- inserting a jump just prior to *that* to ensure that program flow
-- jumps over the constant, like so:
--
--       LDA =...=
--
--  becomes
--
--       JMP *+2
--       CON ...
--       LDA *-1
--
-- This is not how Knuth describes it and it has the (potentially
-- serious) downside that the MIX programmer cannot reason about the
-- size of a program segment (i.e. the sequence of instructions
-- between ORIG directives).  Ultimately, this needs to be fixed so
-- that we actually support "forward references" in the style
-- described in TAOCP; that is going to require the assembler routine
-- to be rewritten to support this, though, since it means we have to
-- evaluate symbols that haven't been defined at the time they are
-- encountered.

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
