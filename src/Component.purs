module Component where

import Prelude

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)

import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..), either)
import Data.Lens (Lens)

newtype Component m v s u = Component ((u -> m Unit) -> s -> v)

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

refocus :: forall m s u v u' s'. Lens s' u' s u -> Component m v s u -> Component m v s' u'
refocus lens = lens

contraHoist :: forall m m' v s u. (m' ~> m) -> Component m v s u -> Component m' v s u
contraHoist nat (Component cmp) = Component \u s -> cmp (nat <<< u) s

