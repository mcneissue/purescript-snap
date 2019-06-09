module Snap.Component where

import Prelude
import Prelude as P

import Data.Either (Either(..), either)
import Data.Lens.Record (prop)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Monoidal (class Comonoidal, class ComonoidalMono, class Cosemigroupal, class CosemigroupalMono, class MonoidalMono, class SemigroupalMono, class Lazy2, switch)
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

instance componentLazy :: Lazy2 (Component m v) where
  defer2 f = Component $ \set s -> runComponent (f unit) set s

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

wrapMC :: forall s u m v. (Tuple (u -> m Unit) s -> v) -> MComponent s u m v
wrapMC = MComponent <<< Component <<< curry

unwrapMC :: forall s u m v. MComponent s u m v -> Tuple (u -> m Unit) s -> v
unwrapMC = runMComponent >>> runComponent >>> uncurry

instance functorMComponent :: Functor (MComponent s u m) where
  map f c = wrapMC $ map f (unwrapMC c)

instance applyMComponent :: Apply (MComponent s u m) where
  apply fab fa = wrapMC $ (unwrapMC fab) <*> (unwrapMC fa)

instance bindMComponent :: Bind (MComponent s u m) where
  bind ma amb = wrapMC $ (unwrapMC ma) >>= (unwrapMC <<< amb)

instance applicativeMComponent :: Applicative (MComponent s u m) where
  pure = wrapMC <<< P.pure

newtype CComponent m u s v = CComponent (Component m v s u)

instance semigroupoidCComponent :: Semigroupoid (CComponent m u) where
  compose (CComponent (Component bc)) (CComponent (Component ab)) = CComponent $ Component $ \set -> bc set <<< ab set

instance categoryMComponent :: Category (CComponent m u) where
  identity = CComponent <<< Component $ const identity

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
