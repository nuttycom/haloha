{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Haloha.Plonk.Types where

import Prelude hiding (Any, Product, Sum)

newtype Rotation = Rotation Int32

data Fixed = Fixed
  deriving (Eq, Show)

data Advice = Advice
  deriving (Eq, Show)

data Aux = Aux
  deriving (Eq, Show)

data Any
  = FixedAny
  | AdviceAny
  | AuxAny
  deriving (Eq, Show)

newtype ColIdx = ColIdx Word64

newtype PermIdx = PermIdx Word64

newtype LookIdx = LookIdx Word64

newtype RowIdx = RowIdx Word64
  deriving (Num, Enum)

newtype PointIdx = PointIdx Word64

data Sum a b = Sum a b

data Product a b = Product a

data Scaled a = Scaled a

data Column c where
  FixedCol :: Word64 -> Column Fixed
  AdviceCol :: Word64 -> Column Advice
  AuxCol :: Word64 -> Column Aux
  AnyCol :: Word64 -> Any -> Column Any

deriving instance Eq (Column c)

toAnyColumn :: Column a -> Column Any
toAnyColumn = \case
  FixedCol i -> AnyCol i FixedAny
  AdviceCol i -> AnyCol i AdviceAny
  AuxCol i -> AnyCol i AuxAny
  AnyCol i a -> AnyCol i a

data Index c where
  FixedIdx :: Word64 -> Index Fixed
  AdviceIdx :: Word64 -> Index Advice
  AuxIdx :: Word64 -> Index Aux
  AnyIdx :: Word64 -> Any -> Index Any

toAnyIdx :: Index a -> Index Any
toAnyIdx = \case
  FixedIdx i -> AnyIdx i FixedAny
  AdviceIdx i -> AnyIdx i AdviceAny
  AuxIdx i -> AnyIdx i AuxAny
  AnyIdx i a -> AnyIdx i a

data Variable
  = Variable
      { column :: Column Advice,
        row :: RowIdx
      }

data Expr f a where
  ColExpr :: Index c -> Expr f c
  SumExpr :: Expr f a -> Expr f b -> Expr f (Sum a b)
  MulExpr :: Expr f a -> Expr f b -> Expr f (Product a b)
  ScaleExpr :: Expr f a -> f -> Expr f (Scaled a)

data AnyExpr f = forall a. AnyExpr (Expr f a)

class Field f where
  fneg :: f -> f
  fzero :: f
  fone :: f
  fsquare :: f -> f
  fplus :: f -> f -> f
