module Control.K where

import Prelude hiding (map, bind, join)
import Effect (Effect)
import Data.Tuple (fst, snd)
import Data.Either (Either(..), either)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Either.Nested (type (\/))

type K i o x = (x -> i) -> o
type K' r x = K r r x

type MK m v = K (m Unit) (m Unit) v
type EK v = MK Effect v

map :: ∀ i o a b. (a -> b) -> K i o a -> K i o b
map f k cb = k $ cb <<< f

mapFlipped :: ∀ i o a b. K i o a -> (a -> b) -> K i o b
mapFlipped = flip map

infixr 4 mapFlipped as <#>

mapO :: ∀ i o o' a. (o -> o') -> K i o a -> K i o' a
mapO f k = f <<< k

mapI :: ∀ i i' o a. (i' -> i) -> K i o a -> K i' o a
mapI f k cb = k $ f <<< cb

pure :: ∀ r x. x -> K r r x
pure x f = f x

join :: ∀ a b c x. K b c (K a b x) -> K a c x
join k cb = k $ \f -> f cb

bind :: ∀ a b c xa xb. (xa -> K a b xb) -> K b c xa -> K a c xb
bind amb = join <<< map amb

-- Sort of represents running two continuations in parallel
parallel :: ∀ i1 o1 x1 i2 o2 x2.
  K i1 o1 x1 /\ K i2 o2 x2 -> K (i1 /\ i2) (o1 /\ o2) (x1 \/ x2)
parallel (k1 /\ k2) cb = k1 (fst <<< cb <<< Left) /\ k2 (snd <<< cb <<< Right)

dup :: ∀ x. x -> x /\ x
dup x = x /\ x

merge :: ∀ x. x \/ x -> x
merge = either identity identity

empty :: ∀ i o x. Monoid o =>
  K i o x
empty = const mempty

diverge :: ∀ i o v1 v2.
  K i o v1 \/ K i o v2 -> K i o (v1 \/ v2)
diverge = either (map Left) (map Right)
