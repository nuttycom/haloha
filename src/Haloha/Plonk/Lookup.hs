module Haloha.Plonk.Lookup where

import Haloha.Plonk.Types (Any, Column)
import Prelude hiding (Any)

data Argument
  = Argument
      { inputColumns :: [Column Any],
        tableColumns :: [Column Any]
      }
