{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Haloha.Plonk.Circuit where

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

import qualified Haloha.Plonk.Lookup as Lookup
import qualified Haloha.Plonk.Permutation as Permutation
import Haloha.Plonk.Types
import Prelude hiding (Any, Product, Sum)

data ColOp a where
  NewAdviceCol :: ColOp (Column Advice)
  NewFixedCol :: ColOp (Column Fixed)
  NewAuxCol :: ColOp (Column Aux)
  ColOpPure :: a -> ColOp a
  ColOpLiftA2 :: (a -> b -> c) -> ColOp a -> ColOp b -> ColOp c

instance Functor ColOp where
  fmap f op = ColOpLiftA2 (\a _ -> f a) op (ColOpPure absurd)

instance Applicative ColOp where
  pure = ColOpPure
  liftA2 = ColOpLiftA2

data CsOp f a where
  BindCols :: ColOp c -> CsOp f c
  NewGate :: Expr f a -> CsOp f ()
  Permutation :: [Column Advice] -> CsOp f PermIdx
  -- Should this be [Column Advice] -> [Column Fixed] -> ...
  Lookup :: [Column Any] -> [Column Any] -> CsOp f LookIdx
  QueryAdvice :: Column Advice -> Rotation -> CsOp f (Expr f Advice)
  QueryAdviceIndex :: Column Advice -> Rotation -> CsOp f AdviceQueryIdx
  QueryFixed :: Column Fixed -> Rotation -> CsOp f (Expr f Fixed)
  QueryFixedIndex :: Column Fixed -> Rotation -> CsOp f FixedQueryIdx
  QueryAux :: Column Aux -> Rotation -> CsOp f (Expr f Aux)
  QueryAuxIndex :: Column Aux -> Rotation -> CsOp f AuxQueryIdx
  QueryAny :: Column Any -> Rotation -> CsOp f (Expr f Any)
  QueryAnyIndex :: Column Any -> Rotation -> CsOp f AnyQueryIdx
  GetAdviceQueryIndex :: Column Advice -> Rotation -> CsOp f AdviceQueryIdx
  GetFixedQueryIndex :: Column Fixed -> Rotation -> CsOp f FixedQueryIdx
  GetAuxQueryIndex :: Column Aux -> Rotation -> CsOp f AuxQueryIdx
  GetAnyQueryIndex :: Column Any -> Rotation -> CsOp f AnyQueryIdx
  CsPure :: a -> CsOp f a
  CsBind :: (a -> CsOp f b) -> CsOp f a -> CsOp f b

instance Functor (CsOp f) where
  fmap f op = CsBind (CsPure . f) op

instance Applicative (CsOp f) where
  pure = CsPure
  liftA2 f opA opB = CsBind (\a -> CsBind (CsPure . f a) opB) opA

instance Monad (CsOp f) where
  (>>=) = flip CsBind

data Error where
  ColumnNotFound :: Column Advice -> Error

data AssignOp f a where
  NewRow :: AssignOp f RowIdx
  AssignAdvice :: Column Advice -> RowIdx -> f -> AssignOp f (Either Error ())
  AssignFixed :: Column Fixed -> RowIdx -> f -> AssignOp f (Either Error ())
  Copy :: PermIdx -> ColIdx -> RowIdx -> ColIdx -> RowIdx -> AssignOp f (Either Error ())
  AssignPure :: a -> AssignOp f a
  AssignBind :: (a -> AssignOp f b) -> AssignOp f a -> AssignOp f b

instance Functor (AssignOp f) where
  fmap f op = AssignBind (AssignPure . f) op

instance Applicative (AssignOp f) where
  pure = AssignPure
  liftA2 f opA opB = AssignBind (\a -> AssignBind (AssignPure . f a) opB) opA

instance Monad (AssignOp f) where
  (>>=) = flip AssignBind

data ConstraintSystem f
  = ConstraintSystem
      { numFixedCols :: Word64,
        numAdviceCols :: Word64,
        numAuxCols :: Word64,
        gates :: [AnyExpr f],
        adviceQueries :: [(Column Advice, Rotation)],
        auxQueries :: [(Column Aux, Rotation)],
        fixedQueries :: [(Column Fixed, Rotation)],
        rotations :: Map Rotation PointIdx,
        permutations :: [Permutation.Argument],
        lookups :: [Lookup.Argument]
      }

class (Field f) => Circuit f c where
  configure :: CsOp f c
  synthesize :: c -> AssignOp f (Either Error ()) -> AssignOp f (Either Error ())
