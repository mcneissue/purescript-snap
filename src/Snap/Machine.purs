module Snap.Machine (module Step, module Snap.Machine) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Either.Nested (type (\/))
import Data.Either (either, Either(..))
import Control.K as K
import Data.Profunctor (lcmap)
import Snap.Machine.Step (Step, Transition(..))
import Snap.Machine.Step as Step

type Machine s i e = s -> Step s i e

-- Effectful machines
type EMachine s i = Machine s i (K.EK i)

type MMachine m s i = Machine s i (m i)

mapI :: ∀ s i i' e. (i' -> i) -> Machine s i e -> Machine s i' e
mapI = map <<< lcmap

mapE :: ∀ s i e e'. (e -> e') -> Machine s i e -> Machine s i e'
mapE = map <<< map <<< map

splice :: forall s1 s2 i1 i2 e1 e2.
  Machine s1 i1 e1 -> Machine s2 i2 e2 -> Machine (s1 /\ s2) (i1 \/ i2) (e1 \/ e2)
splice m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  step1 /\ step2 -> either
    (Step.foldTransition (\s1' e1 -> Yes (s1' /\ s2 ) $ Left  e1) No <<< step1)
    (Step.foldTransition (\s2' e2 -> Yes (s1  /\ s2') $ Right e2) No <<< step2)

esplice :: forall s1 s2 i1 i2.
  EMachine s1 i1 -> EMachine s2 i2 -> EMachine (s1 /\ s2) (i1 \/ i2)
esplice m1 m2 = mapE K.diverge $ splice m1 m2
