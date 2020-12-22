{-# LANGUAGE GADTs #-}

module Haloha.Plonk.Types where

import Prelude hiding (Any, Product, Sum)

newtype Rotation = Rotation Int32

newtype Index = Index Word64

newtype PermIdx = PermIdx Word64

newtype ColIdx = ColIdx Word64

newtype RowIdx = RowIdx Word64

newtype PointIdx = PointIdx Word64

data Fixed = Fixed

data Advice = Advice

data Aux = Aux

data Sum a b = Sum a b

data Product a b = Product a

data Scaled a = Scaled a

data Any
  = FixedAny
  | AdviceAny
  | AuxAny

data Column c
  = Column
      { index :: Index,
        columnType :: c
      }

data Expr f (m :: * -> *) a where
  FixedExpr :: Index -> Expr f m Fixed
  AdviceExpr :: Index -> Expr f m Advice
  AuxExpr :: Index -> Expr f m Aux
  SumExpr :: Expr f m a -> Expr f m b -> Expr f m (Sum a b)
  ProductExpr :: Expr f m a -> Expr f m b -> Expr f m (Product a b)
  ScaledExpr :: Expr f m a -> f -> Expr f m (Scaled a)

data AnyExpr f m = forall a. AnyExpr (Expr f m a)
