module Snap where

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap)
import Snap.SYTC.Component (Cmp)

type Snapper m s u = { put :: u -> m Unit, get :: m s }

encapsulate :: forall m v s u. Monad m => Snapper m s u -> Cmp m v s u -> Cmp m (m v) Unit Void
encapsulate { get, put } cmp _ _ = get <#> cmp put

newtype Target m v = Target (v -> m (Target m v))

instance contravariantTarget :: Functor m => Contravariant (Target m) where
  cmap f (Target a) = Target \v -> cmap f <$> a (f v)

snap :: forall m v x
      . Monad m
     => Cmp m v Unit Void
     -> Target m v
     -> m x
snap cmp t = loop t
  where
  v = cmp absurd unit
  loop (Target render) = do
    t' <- render v
    loop t'
