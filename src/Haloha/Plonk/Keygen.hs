module Haloha.Plonk.Keygen where

data Params c
  = Params
      { k :: Word32,
        n :: Word64,
        g :: [c],
        g_lagrange :: [c],
        h :: c
      }
