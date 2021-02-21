module Snap.Machine.Transition where

import Prelude hiding (pure, join)

import Data.Tuple.Nested (type (/\), (/\))
import Data.Bifunctor (class Bifunctor)

data Transition s e = No | Yes s e

transition :: ∀ s e r. r -> (s -> e -> r) -> Transition s e -> r
transition no yes = case _ of
  No -> no
  Yes s e -> yes s e

instance functorTransition :: Functor (Transition s) where
  map f = transition No $ \s -> Yes s <<< f

instance bifunctorTransition :: Bifunctor Transition where
  bimap f g = transition No $ \a b -> Yes (f a) (g b)

join :: ∀ s1 s2 e. Transition s1 (Transition s2 e) -> Transition (s1 /\ s2) e
join No = No
join (Yes _ No) = No
join (Yes s1 (Yes s2 e)) = Yes (s1 /\ s2) e

pure :: ∀ e. e -> Transition Unit e
pure = Yes unit

kcompose :: ∀ s1 s2 a b c. (a -> Transition s1 b) -> (b -> Transition s2 c) -> a -> Transition (s1 /\ s2) c
kcompose f g = join <<< map g <<< f

