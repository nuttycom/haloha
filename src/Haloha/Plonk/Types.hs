{-# LANGUAGE GADTs #-}

module Haloha.Plonk.Types where

import Prelude hiding (Any, Product, Sum)

newtype Rotation = Rotation Int32

newtype Index = Index Word64

newtype ColIdx = ColIdx Word64

newtype PermIdx = PermIdx Word64

newtype LookIdx = LookIdx Word64

newtype RowIdx = RowIdx Word64

newtype PointIdx = PointIdx Word64

newtype FixedQueryIdx = FixedQueryIdx Word64

newtype AdviceQueryIdx = AdviceQueryIdx Word64

newtype AuxQueryIdx = AuxQueryIdx Word64

data AnyQueryIdx
  = AnyFixed FixedQueryIdx
  | AnyAdvice AdviceQueryIdx
  | AnyAux AuxQueryIdx

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

fixedToAny :: Column Fixed -> Column Any
fixedToAny c = Column { index = index c, columnType = FixedAny }

adviceToAny :: Column Advice -> Column Any
adviceToAny c = Column { index = index c, columnType = AdviceAny }

auxToAny :: Column Aux -> Column Any
auxToAny c = Column { index = index c, columnType = AuxAny }

data Variable
  = Variable
      { column :: Column Advice,
        row :: RowIdx
      }

data Expr f a where
  FixedExpr :: Index -> Expr f Fixed
  AdviceExpr :: Index -> Expr f Advice
  AuxExpr :: Index -> Expr f Aux
  SumExpr :: Expr f a -> Expr f b -> Expr f (Sum a b)
  MulExpr :: Expr f a -> Expr f b -> Expr f (Product a b)
  ScaleExpr :: Expr f a -> f -> Expr f (Scaled a)

data AnyExpr f = forall a. AnyExpr (Expr f a)

class Field f where
  fneg :: f -> f
  fone :: f
