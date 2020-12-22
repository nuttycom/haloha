module Haloha.Plonk.Test where

import Haloha.Plonk.Circuit
import Haloha.Plonk.Types

data PlonkConfig
  = PlonkConfig
      { _a :: Column Advice,
        _b :: Column Advice,
        _c :: Column Advice,
        _d :: Column Advice,
        _e :: Column Advice,
        _sa :: Column Fixed,
        _sb :: Column Fixed,
        _sc :: Column Fixed,
        _sm :: Column Fixed,
        _sp :: Column Fixed,
        _sl :: Column Fixed,
        _sl2 :: Column Fixed,
        _perm :: PermIdx,
        _perm2 :: PermIdx
      }

configure :: Field f => CsOp f m PlonkConfig
configure = do
  e <- NewAdviceCol
  a <- NewAdviceCol
  b <- NewAdviceCol
  sf <- NewFixedCol
  c <- NewAdviceCol
  d <- NewAdviceCol
  p <- NewAuxCol

  perm <- Permutation [a, b, c]
  perm2 <- Permutation [a, b, c]

  sm <- NewFixedCol
  sa <- NewFixedCol
  sb <- NewFixedCol
  sc <- NewFixedCol
  sp <- NewFixedCol
  sl <- NewFixedCol
  sl2 <- NewFixedCol

  _ <- Lookup [adviceToAny a] [fixedToAny sl]
  _ <- Lookup (adviceToAny <$> [a, b]) (fixedToAny <$> [sl, sl2])

  let r0 = Rotation 0
  dq <- QueryAdvice d (Rotation 1)
  aq <- QueryAdvice a r0
  sfq <- QueryFixed sf (Rotation 0)
  eq <- QueryAdvice e r0
  bq <- QueryAdvice b r0
  cq <- QueryAdvice c r0

  saq <- QueryFixed sa r0
  sbq <- QueryFixed sb r0
  scq <- QueryFixed sc r0
  smq <- QueryFixed sm r0

  _ <- NewGate
    (SumExpr
      (MulExpr aq saq)
      (SumExpr
        (MulExpr bq sbq)
        (SumExpr
          (MulExpr aq (MulExpr bq smq))
          (SumExpr
            (MulExpr cq (ScaleExpr scq (fneg fone)))
            (MulExpr sfq (MulExpr dq eq))
          )
        )
      )
    )

  pq <- QueryAux p r0
  spq <- QueryFixed sp r0

  _ <- NewGate (MulExpr spq (SumExpr aq (ScaleExpr pq (fneg fone))))

  pure $ PlonkConfig {
    _a = a,
    _b = b,
    _c = c,
    _d = d,
    _e = e,
    _sa = sa,
    _sb = sb,
    _sc = sc,
    _sm = sm,
    _sp = sp,
    _sl = sl,
    _sl2 = sl2,
    _perm = perm,
    _perm2 = perm2
  }

