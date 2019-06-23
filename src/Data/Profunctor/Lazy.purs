module Data.Profunctor.Lazy where

import Prelude

-- TODO: If we do end up using this, we need to actually do the memoization stuff Lazy does
class Lazy2 p where
  defer2 :: forall x y. (Unit -> p x y) -> p x y
