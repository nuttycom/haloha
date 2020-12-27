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
import Prelude hiding (Any, Product, Sum, init)

data ColOp a where
  NewAdviceCol :: ColOp (Column Advice)
  NewFixedCol :: ColOp (Column Fixed)
  NewAuxCol :: ColOp (Column Aux)
  -- Replace with FreeAp (Coyoneda ColOp)
  ColOpPure :: a -> ColOp a
  ColOpLiftA2 :: (a -> b -> c) -> ColOp a -> ColOp b -> ColOp c

instance Functor ColOp where
  fmap f op = ColOpLiftA2 (\a _ -> f a) op (ColOpPure absurd)

instance Applicative ColOp where
  pure = ColOpPure
  liftA2 = ColOpLiftA2

data QueryOp a where
  QueryIndex :: Column c -> Rotation -> QueryOp (Index c)
  -- Replace with FreeAp (Coyoneda QueryOp)
  QueryOpPure :: a -> QueryOp a
  QueryOpLiftA2 :: (a -> b -> c) -> QueryOp a -> QueryOp b -> QueryOp c

instance Functor QueryOp where
  fmap f op = QueryOpLiftA2 (\a _ -> f a) op (QueryOpPure absurd)

instance Applicative QueryOp where
  pure = QueryOpPure
  liftA2 = QueryOpLiftA2

queryAnyIndex :: Column Any -> Rotation -> QueryOp (Index Any)
queryAnyIndex (AnyCol i ctype) rot = case ctype of
  FixedAny -> toAnyIdx <$> QueryIndex (FixedCol i) rot
  AdviceAny -> toAnyIdx <$> QueryIndex (AdviceCol i) rot
  AuxAny -> toAnyIdx <$> QueryIndex (AuxCol i) rot

getColumnExpr :: Column c -> Rotation -> QueryOp (Expr f c)
getColumnExpr col rot = ColExpr <$> QueryIndex col rot

data CsOp f a where
  NewGate :: Expr f a -> CsOp f ()
  NewPermutation :: [Column Advice] -> CsOp f PermIdx
  NewLookup :: [(Column Any, Column Any)] -> CsOp f LookIdx
  EvalQuery :: QueryOp a -> CsOp f a
  -- Replace with Free (Coyoneda CsOp)
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

data SynthOp f a where
  NewRow :: SynthOp f RowIdx
  AssignAdvice :: Column Advice -> RowIdx -> f -> SynthOp f (Either Error ())
  AssignFixed :: Column Fixed -> RowIdx -> f -> SynthOp f (Either Error ())
  Copy :: PermIdx -> ColIdx -> RowIdx -> ColIdx -> RowIdx -> SynthOp f (Either Error ())
  AssignPure :: a -> SynthOp f a
  AssignBind :: (a -> SynthOp f b) -> SynthOp f a -> SynthOp f b

instance Functor (SynthOp f) where
  fmap f op = AssignBind (AssignPure . f) op

instance Applicative (SynthOp f) where
  pure = AssignPure
  liftA2 f oa ob = AssignBind (\b -> fmap (flip f b) oa) ob

instance Monad (SynthOp f) where
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

data CircuitBuilder f
  = forall cols exprs cfg.
    CircuitBuilder
      { init :: ColOp cols,
        query :: cols -> QueryOp exprs,
        configure :: cols -> exprs -> CsOp f cfg,
        synthesize :: cfg -> SynthOp f (Either Error ())
      }

instance Semigroup (CircuitBuilder f) where
  (CircuitBuilder i1 q1 c1 s1) <> (CircuitBuilder i2 q2 c2 s2) =
    CircuitBuilder
      { init = (,) <$> i1 <*> i2,
        query = \(cols1, cols2) -> (,) <$> q1 cols1 <*> q2 cols2,
        configure = \(cols1, cols2) (exprs1, exprs2) -> (,) <$> c1 cols1 exprs1 <*> c2 cols2 exprs2,
        synthesize = \(cfg1, cfg2) -> s1 cfg1 *> s2 cfg2
      }
