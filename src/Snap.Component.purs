module Snap.Component where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Prim.Row (class Cons)
import Type.Prelude (SProxy)
import Type.Row.Homogeneous (class Homogeneous)

newtype Component m v s u = Component ((u -> m Unit) -> s -> v)

type Component' m v s = Component m v s s

runComponent :: forall m v s u. Component m v s u -> ((u -> m Unit) -> s -> v)
runComponent (Component cmp) = cmp

switch :: forall m v a b c d. Component m v a b -> Component m v c d -> Component m v (Either a c) (Either b d)
switch (Component f) (Component g) = Component \set -> either (f $ set <<< Left) (g $ set <<< Right)

instance semigroupComponent :: Semigroup v => Semigroup (Component m v s u) where
  append (Component a) (Component b) = Component \u s -> a u s <> b u s

instance monoidComponent :: Monoid v => Monoid (Component m v s u) where
  mempty = Component \_ _ -> mempty

instance profunctorComponent :: Profunctor (Component m v) where
  dimap :: forall a b c d m v. (a -> b) -> (c -> d) -> Component m v b c -> Component m v a d
  dimap f g (Component cmp) = Component \u s -> cmp (u <<< g) (f s)

instance strongComponent :: Strong (Component m v) where
  first (Component cmp)  = Component \u s -> cmp (u <<< (\b -> Tuple b (snd s))) (fst s)
  second (Component cmp) = Component \u s -> cmp (u <<< (\b -> Tuple (fst s) b)) (snd s)

instance choiceComponent :: Monoid v => Choice (Component m v) where
  left (Component cmp) = Component \u -> either (cmp (u <<< Left)) (const mempty)
  right (Component cmp) = Component \u -> either (const mempty) (cmp (u <<< Right))

bind :: forall m v v' s u. Component m v s u -> (v -> Component m v' s u) -> Component m v' s u
bind (Component cmp) fn = Component \u s -> runComponent (fn $ cmp u s) u s

pure :: forall m v s u. v -> Component m v s u
pure v = Component \_ _ -> v

contraHoist :: forall m m' v s u. (m' ~> m) -> Component m v s u -> Component m' v s u
contraHoist nat (Component cmp) = Component \set s -> cmp (nat <<< set) s

contraHoistVoid :: forall m m' v s u. (forall a. m' a -> m Unit) -> Component m v s u -> Component m' v s u
contraHoistVoid nat (Component cmp) = Component \set s -> cmp (nat <<< set) s

data FocusProp = FocusProp

instance focusProp :: (IsSymbol s, Strong p, Cons s a r r1, Cons s b r r2) => MappingWithIndex FocusProp (SProxy s) (p a b) (p { | r1 } { | r2 }) where
  mappingWithIndex FocusProp s c = prop s c

refocusAll :: forall a b. HMapWithIndex FocusProp a b => a -> b
refocusAll = hmapWithIndex FocusProp

squash :: forall r v. HFoldl (v -> v -> v) v { | r } v => Homogeneous r v => Monoid v => { | r } -> v
squash r = hfoldl ((<>) :: v -> v -> v) (mempty :: v) r

handle :: forall m v s u. (u -> s -> m Unit) -> Component m v s u -> Component' m v s
handle f (Component c) = Component \set s -> c (flip f s) s
