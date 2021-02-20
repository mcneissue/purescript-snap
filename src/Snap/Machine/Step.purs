module Snap.Machine.Step where

import Prelude

import Data.Bifunctor (class Bifunctor, lmap)

data Transition s e = Yes s e | No

instance functorTransition :: Functor (Transition s) where
  map f No = No
  map f (Yes s e) = Yes s $ f e

instance bifunctorTransition :: Bifunctor Transition where
  bimap _ _ No = No
  bimap f g (Yes s e) = Yes (f s) (g e)

foldTransition :: forall s e r. (s -> e -> r) -> r -> Transition s e -> r
foldTransition yes no = case _ of
  No -> no
  Yes s e -> yes s e

type Step s i e = i -> Transition s e

mapS :: forall s s' i e. (s -> s') -> Step s i e -> Step s' i e
mapS f s i = lmap f $ s i
