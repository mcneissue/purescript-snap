module Snap where

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap)
import Snap.Component.SYTC (Cmp)
import Snap.Snapper (Snapper(..))

encapsulate :: forall m v s u. Monad m => Snapper m u s -> Cmp m v s u -> Cmp m (m v) Unit Void
encapsulate (Snapper { get, put }) cmp _ _ = get <#> cmp put

newtype Target m v = Target (v -> m (Target m v))

instance contravariantTarget :: Functor m => Contravariant (Target m) where
  cmap f (Target a) = Target \v -> cmap f <$> a (f v)

hoistTarget :: forall m n v. Functor n => (m ~> n) -> Target m v -> Target n v
hoistTarget n = go
  where
  go (Target t) = Target $ map go <<< n <<< t

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
