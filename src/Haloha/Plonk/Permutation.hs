module Haloha.Plonk.Permutation where

import Haloha.Plonk.Types (Advice, Column)

newtype Argument = Argument [Column Advice]
