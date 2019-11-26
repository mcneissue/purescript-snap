module Snap where

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap)
import Snap.SYTC.Component (Cmp)

newtype Target m v = Target (v -> m (Target m v))

instance contravariantTarget :: Functor m => Contravariant (Target m) where
  cmap f (Target a) = Target \v -> cmap f <$> a (f v)

type Snapper m s u = { put :: u -> m Unit, get :: m s }

snap :: forall m v s u
      . Monad m
     => Snapper m s u
     -> Cmp m v s u
     -> Target m v
     -> m Unit
snap { put, get } cmp t = loop t
  where
  loop (Target render) = do
    s <- get
    let v = cmp put s
    t' <- render v
    loop t'
