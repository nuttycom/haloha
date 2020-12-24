{-# LANGUAGE MultiParamTypeClasses #-}

module Haloha.Plonk2.Circuit where

import Haloha.Plonk.Types

data Request a where
  NewAdvice :: Request Advice
  NewFixed :: Request Fixed
  NewAux :: Request Aux

-- | c is the functor for a configuration type
class (Field f) => Circuit f (conf :: (* -> *) -> *) where
  allocate :: conf Request -> conf Column
  configure :: conf Column -> CsOp f c
  synthesize :: c -> AssignOp f (Either Error ()) -> AssignOp f (Either Error ())
