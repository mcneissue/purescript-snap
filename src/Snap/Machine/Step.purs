module Snap.Machine.Step where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Bifunctor (class Bifunctor, lmap)

data Transition s e = No | Yes s e

joinish :: ∀ s1 s2 e. Transition s1 (Transition s2 e) -> Transition (s1 /\ s2) e
joinish No = No
joinish (Yes _ No) = No
joinish (Yes s1 (Yes s2 e)) = Yes (s1 /\ s2) e

pureish :: ∀ e. e -> Transition Unit e
pureish = Yes unit

kcomposeish :: ∀ s1 s2 a b c. (a -> Transition s1 b) -> (b -> Transition s2 c) -> a -> Transition (s1 /\ s2) c
kcomposeish f g = joinish <<< map g <<< f

instance functorTransition :: Functor (Transition s) where
  map f No = No
  map f (Yes s e) = Yes s $ f e

instance bifunctorTransition :: Bifunctor Transition where
  bimap _ _ No = No
  bimap f g (Yes s e) = Yes (f s) (g e)

foldTransition :: ∀ s e r. (s -> e -> r) -> r -> Transition s e -> r
foldTransition yes no = case _ of
  No -> no
  Yes s e -> yes s e

type Step s i e = i -> Transition s e

mapS :: ∀ s s' i e. (s -> s') -> Step s i e -> Step s' i e
mapS f s i = lmap f $ s i
