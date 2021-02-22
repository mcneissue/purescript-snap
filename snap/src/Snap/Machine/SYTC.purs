module Snap.Machine.SYTC where

import Prelude hiding (unit)

import Control.Biapply ((<<*>>))
import Control.K as K
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Profunctor (lcmap)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Snap.Machine.Step (Step)
import Snap.Machine.Transition (Transition(..))
import Snap.Machine.Transition (transition, pure, kcompose) as Transition
import Snap.Machine.Step (mapS) as Step

type Machine s i e = s -> Step s i e

-- Effectful machines
type EMachine s i = Machine s i (K.EK i)

type MMachine m s i = Machine s i (m i)

mapS :: ∀ s s' e i. (s -> s') -> (s' -> s) -> Machine s i e -> Machine s' i e
mapS f g = (map <<< map <<< lmap $ f) <<< lcmap g

mapI :: ∀ s i i' e. (i' -> i) -> Machine s i e -> Machine s i' e
mapI = map <<< lcmap

mapE :: ∀ s i e e'. (e -> e') -> Machine s i e -> Machine s i e'
mapE = map <<< map <<< map

splice :: ∀ s1 s2 i1 i2 e1 e2.
  Machine s1 i1 e1 -> Machine s2 i2 e2 -> Machine (s1 /\ s2) (i1 \/ i2) (e1 \/ e2)
splice m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  step1 /\ step2 -> either
    (Transition.transition No (\s1' e1 -> Yes (s1' /\ s2 ) (Left e1 )) <<< step1)
    (Transition.transition No (\s2' e2 -> Yes (s1  /\ s2') (Right e2)) <<< step2)

unit :: ∀ e. Machine Unit Void e
unit = const absurd

compose :: ∀ s1 s2 a b c. Machine s1 a b /\ Machine s2 b c -> Machine (s1 /\ s2) a c
compose m1m2 = uncurry Transition.kcompose <<< (m1m2 <<*>> _)

identity :: ∀ a. Machine Unit a a
identity = pure Transition.pure

reportState :: ∀ s i e. Machine s i e -> Machine s i (s /\ e)
reportState m s i = Transition.transition No (\s' e -> Yes s' (s' /\ e)) $ m s i

newtype Behavior i e = Behavior (Step (Behavior i e) i e)

unfold :: ∀ s i e. Machine s i e -> s -> Behavior i e
unfold m = go
  where
  go s = Behavior (Step.mapS go $ m s)
