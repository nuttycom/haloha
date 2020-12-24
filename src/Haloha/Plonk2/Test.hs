module Haloha.Plonk2.Test where

data PlonkCols (c :: * -> *)
  = PlonkCols
      { _a :: c Advice,
        _b :: c Advice,
        _c :: c Advice,
        _d :: c Advice,
        _e :: c Advice,
        _sa :: c Fixed,
        _sb :: c Fixed,
        _sc :: c Fixed,
        _sf :: c Fixed,
        _sm :: c Fixed,
        _sp :: c Fixed,
        _sl :: c Fixed,
        _sl2 :: c Fixed,
        _p :: c Aux
      }
