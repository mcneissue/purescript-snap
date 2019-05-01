module Snap where

import Prelude

import Component (Component(..))

newtype Target m v = Target (m Unit -> v -> m Unit)

type Snapper m s u = { put :: u -> m Unit, get :: m s }

snap :: forall m v s u
      . Monad m
     => Snapper m s u
     -> Component m v s u
     -> Target m v 
     -> m Unit
snap { put, get } (Component cmp) t = loop t
  where
    loop (Target render) = do
        s <- get
        let v = cmp put s
        render (loop t) v