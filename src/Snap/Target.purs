module Snap.Target where

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap)

newtype Target m v = Target (v -> m (Target m v))

instance contravariantTarget :: Functor m => Contravariant (Target m) where
  cmap f (Target a) = Target \v -> cmap f <$> a (f v)

hoistTarget :: forall m n v. Functor n => (m ~> n) -> Target m v -> Target n v
hoistTarget n = go
  where
  go (Target t) = Target $ map go <<< n <<< t
