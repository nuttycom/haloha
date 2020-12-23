module Haloha.Plonk.Test where

import Control.Error.Util (note)
import Control.Monad.Except (liftEither)
import Haloha.Plonk.Circuit
import Haloha.Plonk.Types

data PlonkCols
  = PlonkCols
      { _a :: Column Advice,
        _b :: Column Advice,
        _c :: Column Advice,
        _d :: Column Advice,
        _e :: Column Advice,
        _sa :: Column Fixed,
        _sb :: Column Fixed,
        _sc :: Column Fixed,
        _sf :: Column Fixed,
        _sm :: Column Fixed,
        _sp :: Column Fixed,
        _sl :: Column Fixed,
        _sl2 :: Column Fixed,
        _p :: Column Aux
      }

data PlonkConfig
  = PlonkConfig
      { _cols :: PlonkCols,
        _perm :: PermIdx,
        _perm2 :: PermIdx
      }

plonkCols :: ColOp PlonkCols
plonkCols =
  PlonkCols
    <$> NewAdviceCol
    <*> NewAdviceCol
    <*> NewAdviceCol
    <*> NewAdviceCol
    <*> NewAdviceCol
    <*> NewFixedCol
    <*> NewFixedCol
    <*> NewFixedCol
    <*> NewFixedCol
    <*> NewFixedCol
    <*> NewFixedCol
    <*> NewFixedCol
    <*> NewFixedCol
    <*> NewAuxCol

configure :: Field f => PlonkCols -> CsOp f PlonkConfig
configure cols@(PlonkCols a b c d e sa sb sc sf sm sp sl sl2 p) = do
  perm <- Permutation [a, b, c]
  perm2 <- Permutation [a, b, c]
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
  _ <-
    NewGate
      ( SumExpr
          (MulExpr aq saq)
          ( SumExpr
              (MulExpr bq sbq)
              ( SumExpr
                  (MulExpr aq (MulExpr bq smq))
                  ( SumExpr
                      (MulExpr cq (ScaleExpr scq (fneg fone)))
                      (MulExpr sfq (MulExpr dq eq))
                  )
              )
          )
      )
  pq <- QueryAux p r0
  spq <- QueryFixed sp r0
  _ <- NewGate (MulExpr spq (SumExpr aq (ScaleExpr pq (fneg fone))))
  pure $
    PlonkConfig
      { _cols = cols,
        _perm = perm,
        _perm2 = perm2
      }

publicInput ::
  forall f.
  (Field f) =>
  PlonkCols ->
  f ->
  AssignOp f (Either Error Variable)
publicInput cfg f = runExceptT $ do
  rowIdx <- lift $ NewRow
  ExceptT $ AssignAdvice (_a cfg) rowIdx f
  ExceptT $ AssignFixed (_sp cfg) rowIdx fone
  pure $ Variable (_a cfg) rowIdx

lookupTable ::
  forall f.
  PlonkCols ->
  [(f, f)] ->
  AssignOp f (Either Error ())
lookupTable cfg values =
  runExceptT $ traverse_ (uncurry assignFixed) values
  where
    assignFixed :: f -> f -> ExceptT Error (AssignOp f) ()
    assignFixed v0 v1 = do
      rowIdx <- lift $ NewRow
      ExceptT $ AssignFixed (_sl cfg) rowIdx v0
      ExceptT $ AssignFixed (_sl2 cfg) rowIdx v1

rawMul :: Field f => PlonkCols -> (f, f, f) -> AssignOp f (Either Error (Variable, Variable, Variable))
rawMul (PlonkCols a b c d e sa sb sc _ sm _ _ _ _) (f0, f1, f2) = runExceptT $ do
  rowIdx <- lift $ NewRow
  ExceptT $ AssignAdvice a rowIdx f0
  ExceptT $ AssignAdvice d rowIdx (fsquare . fsquare $ f0)
  ExceptT $ AssignAdvice b rowIdx f1
  ExceptT $ AssignAdvice e rowIdx (fsquare . fsquare $ f1)
  ExceptT $ AssignAdvice c rowIdx f2
  ExceptT $ AssignFixed sa rowIdx fzero
  ExceptT $ AssignFixed sb rowIdx fzero
  ExceptT $ AssignFixed sc rowIdx fone
  ExceptT $ AssignFixed sm rowIdx fone
  pure $ (Variable a rowIdx, Variable b rowIdx, Variable c rowIdx)

rawAdd :: Field f => PlonkCols -> (f, f, f) -> AssignOp f (Either Error (Variable, Variable, Variable))
rawAdd (PlonkCols a b c d _ sa sb sc _ sm _ _ _ _) (f0, f1, f2) = runExceptT $ do
  rowIdx <- lift $ NewRow
  ExceptT $ AssignAdvice a rowIdx f0
  ExceptT $ AssignAdvice d rowIdx (fsquare . fsquare $ f0)
  ExceptT $ AssignAdvice b rowIdx f1
  ExceptT $ AssignAdvice c rowIdx f2
  ExceptT $ AssignFixed sa rowIdx fone
  ExceptT $ AssignFixed sb rowIdx fone
  ExceptT $ AssignFixed sc rowIdx fone
  ExceptT $ AssignFixed sm rowIdx fzero
  pure $ (Variable a rowIdx, Variable b rowIdx, Variable c rowIdx)


copy :: PlonkConfig -> Variable -> Variable -> AssignOp f (Either Error ())
copy cfg left right = runExceptT $ do
  let cidx :: Variable -> Maybe ColIdx
      cidx v = case column v of
        x | x == (_a . _cols) cfg -> Just (ColIdx 0)
        x | x == (_b . _cols) cfg -> Just (ColIdx 1)
        x | x == (_c . _cols) cfg -> Just (ColIdx 2)
        _ -> Nothing
  leftCol <- liftEither $ note (ColumnNotFound (column left)) (cidx left)
  rightCol <- liftEither $ note (ColumnNotFound (column right)) (cidx right)
  ExceptT $ Copy (_perm cfg) leftCol (row left) rightCol (row right)
  ExceptT $ Copy (_perm2 cfg) leftCol (row left) rightCol (row right)

synthesize :: (Field f) => PlonkConfig -> AssignOp f (Either Error ()) -> AssignOp f (Either Error ())
synthesize = undefined
