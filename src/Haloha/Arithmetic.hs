{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Haloha.Arithmetic
  (
  )
where

import Algebra.Field as Field
import Algebra.VectorSpace as Group
import Basement.Types.Word128 (Word128 (..))
import Control.Lens.TH (makePrisms)
import Data.Foldable (Foldable)
import Prelude (undefined)

newtype Challenge = Challenge Word128

makePrisms ''Challenge
-- batchInvert
--   :: (Field.C f, Foldable t)
--   => t f
--   -> f
-- batchInvert items =
--   undefined
