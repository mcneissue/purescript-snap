module Snap.Component where

import Prelude

import Data.Either (Either(..), either)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Monoidal (class Comonoidal, class ComonoidalMono, class Cosemigroupal, class CosemigroupalMono, class MonoidalMono, class SemigroupalMono, switch)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), curry, fst, snd, uncurry)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Prim.Row (class Cons)
import Type.Prelude (SProxy)
import Type.Row.Homogeneous (class Homogeneous)

newtype Component m v s u = Component ((u -> m Unit) -> s -> v)

type Component' m v s = Component m v s s

runComponent :: forall m v s u. Component m v s u -> ((u -> m Unit) -> s -> v)
runComponent (Component cmp) = cmp

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

instance cosemigroupalMonoComponent :: CosemigroupalMono (Component m v) where
  switchMono = switch

instance cosemigroupalComponent :: Cosemigroupal (Component m v) where
  switch (Component f) (Component g) = Component \set -> either (f $ set <<< Left) (g $ set <<< Right)

instance comonoidalMonoComponent :: ComonoidalMono (Component m v) where
  never = Component $ const absurd

instance comonoidalComponent :: Comonoidal (Component m v)

instance semigroupalMonoComponent :: Monoid v => SemigroupalMono (Component m v) where
  zipMono (Component f) (Component g) = Component \set (Tuple a c) -> f (\b -> set $ Tuple b c) a <> g (\d -> set $ Tuple a d) c

instance monoidalMonoComponent :: Monoid v => MonoidalMono (Component m v) where
  infinite = Component \_ _ -> mempty


contraHoist :: forall m' m v s u. (m Unit -> m' Unit) -> Component m' v s u -> Component m v s u
contraHoist nat (Component cmp) = Component \set s -> cmp (nat <<< set) s

newtype MComponent s u m v = MComponent (Component m v s u)

runMComponent :: forall m v s u. MComponent s u m v -> Component m v s u
runMComponent (MComponent c) = c

wrap :: forall s u m v. (Tuple (u -> m Unit) s -> v) -> MComponent s u m v
wrap = MComponent <<< Component <<< curry

unwrap :: forall s u m v. MComponent s u m v -> Tuple (u -> m Unit) s -> v
unwrap = runMComponent >>> runComponent >>> uncurry

instance functorMComponent :: Functor (MComponent s u m) where
  map f c = wrap $ map f (unwrap c)

instance applyMComponent :: Apply (MComponent s u m) where
  apply fab fa = wrap $ (unwrap fab) <*> (unwrap fa)

instance bindMComponent :: Bind (MComponent s u m) where
  bind ma amb = wrap $ (unwrap ma) >>= (unwrap <<< amb)

instance applicativeMComponent :: Applicative (MComponent s u m) where
  pure x = wrap (\_ -> x) -- TODO: Work out why I can't write this as wrap $ pure x

handle :: forall m v s u. (u -> s -> m Unit) -> Component m v s u -> Component' m v s
handle f (Component c) = Component \set s -> c (flip f s) s

data FocusProp = FocusProp

instance focusProp :: (IsSymbol s, Strong p, Cons s a r r1, Cons s b r r2) => MappingWithIndex FocusProp (SProxy s) (p a b) (p { | r1 } { | r2 }) where
  mappingWithIndex FocusProp s c = prop s c

focus :: forall a r v. HMapWithIndex FocusProp a { | r } => HFoldl (v -> v -> v) v { | r } v => Homogeneous r v => Monoid v => a -> v
focus = hmapWithIndex FocusProp >>> hfoldl ((<>) :: v -> v -> v) (mempty :: v)

combine :: forall a r v.
     HMapWithIndex FocusProp a { | r }
  => HFoldl (v -> v -> v) v { | r } v
  => Homogeneous r v
  => Monoid v
  => v -> a -> v
combine a b = a <> focus b

infixl 1 combine as :<>: