module Snap.Machine.Step where

import Prelude

data Transition s e = Yes s e | No

instance functorTransition :: Functor (Transition s) where
  map f No = No
  map f (Yes s e) = Yes s $ f e

foldTransition :: forall s e r. (s -> e -> r) -> r -> Transition s e -> r
foldTransition yes no = case _ of
  No -> no
  Yes s e -> yes s e

type Step s i e = i -> Transition s e
