module Haloha.Arithmetic.Fields.Types
  ( Field (..),
    Choice,
  )
where

-- TODO: actual constant-time boolean?
type Choice = Bool

class Field f where
  zero :: f
  one :: f
  neg :: f -> f
  sub :: f -> f -> f
  add :: f -> f -> f
  mul :: f -> f -> f
  double :: f -> f
  square :: f -> f
  sqrt :: f -> Maybe f

  -- | Multiplicative inverse
  inverse :: f -> Maybe f
