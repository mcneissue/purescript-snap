module Snap.Machine.SYTC where

import Prelude hiding (unit)

import Control.K as K
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Profunctor (lcmap)
import Data.Tuple.Nested (type (/\), (/\))
import Snap.Machine.Step (Step, Transition(..))
import Snap.Machine.Step (Step, Transition(..), foldTransition) as Step

type Machine s i e = s -> Step s i e

-- Effectful machines
type EMachine s i = Machine s i (K.EK i)

type MMachine m s i = Machine s i (m i)

mapS :: forall s s' e i. (s -> s') -> (s' -> s) -> Machine s i e -> Machine s' i e
mapS f g = (map <<< map <<< lmap $ f) <<< lcmap g

mapI :: ∀ s i i' e. (i' -> i) -> Machine s i e -> Machine s i' e
mapI = map <<< lcmap

mapE :: ∀ s i e e'. (e -> e') -> Machine s i e -> Machine s i e'
mapE = map <<< map <<< map

splice :: forall s1 s2 i1 i2 e.
  Machine s1 i1 e -> Machine s2 i2 e -> Machine (s1 /\ s2) (i1 \/ i2) e
splice m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  step1 /\ step2 -> either
    (Step.foldTransition (\s1' e -> Yes (s1' /\ s2 ) e) No <<< step1)
    (Step.foldTransition (\s2' e -> Yes (s1  /\ s2') e) No <<< step2)

unit :: forall e. Machine Unit Void e
unit = const absurd
