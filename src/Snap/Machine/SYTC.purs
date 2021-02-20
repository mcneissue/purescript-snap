module Snap.Machine.SYTC where

import Prelude hiding (unit)

import Control.K as K
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Profunctor (lcmap)
import Data.Tuple.Nested (type (/\), (/\))
import Snap.Machine.Step (Step, Transition(..))
import Snap.Machine.Step (Step, Transition(..), foldTransition, mapS) as Step

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

splice :: forall s1 s2 i1 i2 e1 e2.
  Machine s1 i1 e1 -> Machine s2 i2 e2 -> Machine (s1 /\ s2) (i1 \/ i2) (e1 \/ e2)
splice m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  step1 /\ step2 -> either
    (Step.foldTransition (\s1' e1 -> Yes (s1' /\ s2 ) (Left e1 )) No <<< step1)
    (Step.foldTransition (\s2' e2 -> Yes (s1  /\ s2') (Right e2)) No <<< step2)

compose :: forall s1 s2 a b c. Machine s1 a b -> Machine s2 b c -> Machine (s1 /\ s2) a c
compose m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  step1 /\ step2 -> \a ->
    Step.foldTransition (\s1' b -> Step.foldTransition (\s2' c -> Yes (s1' /\ s2') c) No $ step2 b) No $ step1 a

unit :: forall e. Machine Unit Void e
unit = const absurd

reportState :: forall s i e. Machine s i e -> Machine s i (s /\ e)
reportState m s i = Step.foldTransition (\s' e -> Yes s' (s' /\ e)) No $ m s i

newtype Behavior i e = Behavior (Step (Behavior i e) i e)

unfold :: forall s i e. Machine s i e -> s -> Behavior i e
unfold m = go
  where
  go s = Behavior (Step.mapS go $ m s)
