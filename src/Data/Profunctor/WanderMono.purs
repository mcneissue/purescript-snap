module Data.Profunctor.WanderMono where

import Prelude

import Data.Const (Const(..))
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Snap.Component (Component(..))

class WanderMono p where
  wanderMono :: forall s a. (forall f. Applicative f => (a -> f a) -> (s -> f s)) -> p a a -> p s s

-- TODO: This is totally wrong, and I suspect there's actually no way to properly do a `WanderMono`
-- for Component
instance componentWanderMono :: Monoid v => WanderMono (Component m v) where
  wanderMono f (Component c) = Component go
    where
    go set s = unwrap $ f (Const <<< c (foo set)) s
      where
      foo = lcmap (f pure s)
