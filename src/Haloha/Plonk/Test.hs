{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Haloha.Plonk.Test where

import Control.Error.Util (note)
import Control.Monad.Except (liftEither)
import Haloha.Plonk.Circuit
import Haloha.Plonk.Types
import Prelude hiding (Num (..))

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

assignPublicInput :: forall f. (Field f) => PlonkCols -> f -> AssignOp f (Either Error Variable)
assignPublicInput cfg f = runExceptT $ do
  rowIdx <- lift $ NewRow
  ExceptT $ AssignAdvice (_a cfg) rowIdx f
  ExceptT $ AssignFixed (_sp cfg) rowIdx fone
  pure $ Variable (_a cfg) rowIdx

assignLookupTable :: forall f. PlonkCols -> [(f, f)] -> AssignOp f (Either Error ())
assignLookupTable cfg values =
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

data MyCircuit f
  = MyCircuit
      { mcA :: f,
        mclookupTables :: [(f, f)]
      }

data PlonkExprs f
  = PlonkExprs
      { _aExpr :: Expr f Advice,
        _bExpr :: Expr f Advice,
        _cExpr :: Expr f Advice,
        _dExpr :: Expr f Advice,
        _eExpr :: Expr f Advice,
        _saExpr :: Expr f Fixed,
        _sbExpr :: Expr f Fixed,
        _scExpr :: Expr f Fixed,
        _sfExpr :: Expr f Fixed,
        _smExpr :: Expr f Fixed,
        _spExpr :: Expr f Fixed,
        _pExpr :: Expr f Aux
      }

mcQuery :: PlonkCols -> QueryOp (PlonkExprs f)
mcQuery (PlonkCols a b c d e sa sb sc sf sm sp _ _ p) = do
  let r0 = Rotation 0
  _aExpr <- getColumnExpr a r0
  _dExpr <- getColumnExpr d (Rotation 1)
  _eExpr <- getColumnExpr e r0
  _bExpr <- getColumnExpr b r0
  _cExpr <- getColumnExpr c r0
  _saExpr <- getColumnExpr sa r0
  _sbExpr <- getColumnExpr sb r0
  _scExpr <- getColumnExpr sc r0
  _sfExpr <- getColumnExpr sf (Rotation 0)
  _smExpr <- getColumnExpr sm r0
  _spExpr <- getColumnExpr sp r0
  _pExpr <- getColumnExpr p r0
  pure $ PlonkExprs {..}

mcConfigure :: Field f => PlonkCols -> PlonkExprs f -> CsOp f PlonkConfig
mcConfigure cols@(PlonkCols {..}) (PlonkExprs {..}) = do
  perm <- NewPermutation [_a, _b, _c]
  perm2 <- NewPermutation [_a, _b, _c]
  _ <- NewLookup [(toAnyColumn _a, toAnyColumn _sl)]
  _ <- NewLookup ((toAnyColumn <$> [_a, _b]) `zip` (toAnyColumn <$> [_sl, _sl2]))
  _ <-
    NewGate $
      _aExpr * _saExpr
        + _bExpr * _sbExpr
        + _aExpr * _bExpr * _smExpr
        + _cExpr * _scExpr ^* fneg fone
        + _sfExpr * _dExpr * _eExpr
  _ <- NewGate $ _spExpr * (_aExpr + _pExpr ^* (fneg fone))
  pure $
    PlonkConfig
      { _cols = cols,
        _perm = perm,
        _perm2 = perm2
      }

mcSynthesize :: (Field f) => MyCircuit f -> PlonkConfig -> AssignOp f (Either Error ())
mcSynthesize circuit cfg = runExceptT $ do
  let a = mcA circuit
      cols = _cols cfg
      doSomething _ = do
        (a0, _, c0) <- ExceptT $ rawMul cols (a, a, fsquare a)
        (a1, b1, _) <- ExceptT $ rawAdd cols (a, fsquare a, fplus a (fsquare a))
        ExceptT $ copy cfg a0 a1
        ExceptT $ copy cfg b1 c0
  void . ExceptT $ assignPublicInput (_cols cfg) (fone `fplus` fone)
  traverse_ doSomething [(0 :: Int) .. 10]
  ExceptT $ assignLookupTable cols (mclookupTables circuit)

myCircuitBuilder ::
  Field f =>
  MyCircuit f ->
  CircuitBuilder f
myCircuitBuilder mc =
  CircuitBuilder
    { init = plonkCols,
      query = mcQuery,
      configure = mcConfigure,
      synthesize = mcSynthesize mc
    }
