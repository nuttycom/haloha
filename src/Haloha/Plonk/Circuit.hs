{-# LANGUAGE GADTs #-}

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

data CsOp f (m :: * -> *) a where
  Permutation :: [Column Advice] -> CsOp f m PermIdx
  -- Should this be [Column Advice] -> [Column Fixed] -> ...
  Lookup :: [Column Any] -> [Column Any] -> CsOp f m LookIdx
  NewAdviceCol :: CsOp f m (Column Advice)
  NewFixedCol :: CsOp f m (Column Fixed)
  NewAuxCol :: CsOp f m (Column Aux)
  NewGate :: Expr f a -> CsOp f m ()
  QueryAdvice :: Column Advice -> Rotation -> CsOp f m (Expr f Advice)
  QueryAdviceIndex :: Column Advice -> Rotation -> CsOp f m AdviceQueryIdx
  QueryFixed :: Column Fixed -> Rotation -> CsOp f m (Expr f Fixed)
  QueryFixedIndex :: Column Fixed -> Rotation -> CsOp f m FixedQueryIdx
  QueryAux :: Column Aux -> Rotation -> CsOp f m (Expr f Aux)
  QueryAuxIndex :: Column Aux -> Rotation -> CsOp f m AuxQueryIdx
  QueryAny :: Column Any -> Rotation -> CsOp f m (Expr f Any)
  QueryAnyIndex :: Column Any -> Rotation -> CsOp f m AnyQueryIdx
  GetAdviceQueryIndex :: Column Advice -> Rotation -> CsOp f m AdviceQueryIdx
  GetFixedQueryIndex :: Column Fixed -> Rotation -> CsOp f m FixedQueryIdx
  GetAuxQueryIndex :: Column Aux -> Rotation -> CsOp f m AuxQueryIdx
  GetAnyQueryIndex :: Column Any -> Rotation -> CsOp f m AnyQueryIdx
  Pure :: a -> CsOp f m a
  LiftA2 :: (a -> b -> c) -> CsOp f m a -> CsOp f m b -> CsOp f m c
  Bind :: (a -> CsOp f m b) -> CsOp f m a -> CsOp f m b

instance Functor (CsOp f m) where
  fmap f op = LiftA2 (\a _ -> f a) op (Pure absurd)

instance Applicative (CsOp f m) where
  pure = Pure
  liftA2 = LiftA2

instance Monad (CsOp f m) where
  (>>=) = flip Bind

data Error

data AssignOp f m a where
  AssignAdvice :: Column Advice -> RowIdx -> m (Either Error f) -> AssignOp f m (Either Error ())
  AssignFixed :: Column Fixed -> RowIdx -> m (Either Error f) -> AssignOp f m (Either Error ())
  Copy :: PermIdx -> ColIdx -> RowIdx -> ColIdx -> RowIdx -> AssignOp f m (Either Error ())

data ConstraintSystem f m
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
