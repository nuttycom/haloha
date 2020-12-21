{-# LANGUAGE GADTs #-}

module Haloha.Circuit where

-- PLONK:
--     * keygen
--         takes as input something that describes the structure of the computation
--             (assume for now: picks the number of fixed/advice columns)
--         takes as input something that describes fixed cell values in a table
--         ---> creates a table of appropriate size, populates it with the fixed values
--         returns a proving key, verifying key
--             (assume for now: proving key just contains fixed columns, verifying key empty)
--     * prover
--         takes as input something that describes the structure of the computation
--         takes as input something that describes the private inputs to the
--         ---> create a table of appropriate size, populate its advice column cells with
--              values as directed by the structure of the computation and the private inputs
--         returns a proof
--     * verifier
--         takes a proof, verifying key, returns a boolean
--
-- PLONK operations
--     keygen<C: Circuit<(), Fixed>, Fixed>(Fixed, C) -> (ProvingKey<Fixed>, VerifyingKey)
--         Circuit<Fixed> needs to communicate to the keygen the structure of the circuit
--         (aka the constraints), the number of columns, etc. and the fixed values that
--         never change between proofs.
--
--     prover<C: Circuit<Advice, Fixed>, Advice, Fixed>(Advice, C, ProvingKey<Fixed>) -> Proof
--         Circuit<Advice> needs to communicate to the prover how to assign
--         "advice" column values, should assume the fixed values are computed already
--
--     verifier(Proof, VerifyingKey) -> bool

import Prelude hiding (Product, Sum, Any)

newtype Rotation = Rotation Int32

newtype Index = Index Word64

newtype PermIdx = PermIdx Word64

newtype ColIdx = ColIdx Word64

newtype RowIdx = RowIdx Word64

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

data CsOp f (m :: * -> *) a where
  Permutation :: [Column Advice] -> CsOp f m Index
  Lookup :: [Column Any] -> [Column Any] -> CsOp f m Index

  NewAdviceCol :: CsOp f m (Column Advice)
  NewFixedCol :: CsOp f m (Column Fixed)
  NewAuxCol :: CsOp f m (Column Aux)
  NewGate :: Expr f m a -> CsOp f m (m ())
  AddRotation :: Rotation -> CsOp f m ()

  QueryAdvice :: Column Advice -> Rotation -> CsOp f m (Expr f m Advice)
  QueryAdviceIndex :: Column Advice -> Rotation -> CsOp f m Index
  QueryFixed :: Column Fixed -> Rotation -> CsOp f m (Expr f m Fixed)
  QueryFixedIndex :: Column Fixed -> Rotation -> CsOp f m Index
  QueryAux :: Column Aux -> Rotation -> CsOp f m (Expr f m Aux)
  QueryAuxIndex :: Column Aux -> Rotation -> CsOp f m Index
  QueryAny :: Column Any -> Rotation -> CsOp f m (Expr f m Any)
  QueryAnyIndex :: Column Any -> Rotation -> CsOp f m Index

  GetAdviceQueryIndex :: Column Advice -> Rotation -> CsOp f m Index
  GetFixedQueryIndex :: Column Fixed -> Rotation -> CsOp f m Index
  GetAuxQueryIndex :: Column Aux -> Rotation -> CsOp f m Index
  GetAnyQueryIndex :: Column Any -> Rotation -> CsOp f m Index

  Pure :: a -> CsOp f m a
  LiftA2 :: (a -> b -> c) -> CsOp f m a -> CsOp f m b -> CsOp f m c

data Error

data AssignOp f m a where
  AssignAdvice :: Column Advice -> RowIdx -> m (Either Error f) -> AssignOp f m (Either Error ())
  AssignFixed :: Column Fixed -> RowIdx -> m (Either Error f) -> AssignOp f m (Either Error ())
  Copy :: PermIdx -> ColIdx -> RowIdx -> ColIdx -> RowIdx -> AssignOp f m (Either Error ())
