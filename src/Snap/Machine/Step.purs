module Snap.Machine.Step where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

type Step i o s = o /\ (i -> s)

mapO :: ∀ a b i s. (a -> b) -> Step i a s -> Step i b s
mapO f (o /\ t) = f o /\ t

mapI :: ∀ a b o s. (a -> b) -> Step b o s -> Step a o s
mapI f (o /\ t) = o /\ t <<< f

mapS :: ∀ a b i o. (a -> b) -> Step i o a -> Step i o b
mapS f (o /\ t) = o /\ f <<< t

dimapIO :: ∀ s i o i' o'. (i' -> i) -> (o -> o') -> Step i o s -> Step i' o' s
dimapIO f g (o /\ t) = g o /\ t <<< f
