module Snap.Machine where

import Prelude

import Snap.Machine.Step as Step
import Snap.Machine.Type (Machine)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Bifunctor (bimap)

mapO :: ∀ a b i s.
  (a -> b) -> Machine s i a -> Machine s i b
mapO f = (Step.mapO f <<< _)

mapI :: ∀ a b o s.
  (a -> b) -> Machine s b o -> Machine s a o
mapI f = (Step.mapI f <<< _)

mapS :: ∀ s s' i o.
  (s' -> s) -> (s -> s') -> Machine s i o -> Machine s' i o
mapS f g m = Step.mapS g <<< m <<< f

lstrength :: ∀ x i o s.
  Machine s i o -> Machine (s /\ x) i o
lstrength m (s /\ x) = case m s of
  o /\ t -> o /\ (_ /\ x) <<< t

rstrength :: ∀ x i o s.
  Machine s i o -> Machine (x /\ s) i o
rstrength m (x /\ s) = case m s of
  o /\ t -> o /\ (x /\ _) <<< t

ultraleft :: ∀ x y i o s.
  Machine s i o -> Machine s (i \/ x) (o \/ y)
ultraleft m s = bimap Left (\f -> either f (const s)) $ m s

ultraright :: ∀ x y i o s.
  Machine s i o -> Machine s (x \/ i) (y \/ o)
ultraright m s = bimap Right (\f -> either (const s) f) $ m s

divergent :: ∀ s i1 i2 o1 o2.
  Machine s i1 o1 \/ Machine s i2 o2 -> Machine s (i1 \/ i2) (o1 \/ o2)
divergent = either ultraleft ultraright

independent :: ∀ s1 s2 i1 i2 o1 o2.
  Machine s1 i1 o1 /\ Machine s2 i2 o2 -> Machine (s1 /\ s2) (i1 /\ i2) (o1 /\ o2)
independent (m1 /\ m2) (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  ((o1 /\ t1) /\ (o2 /\ t2)) -> (o1 /\ o2) /\ bimap t1 t2

splice :: ∀ s1 s2 i1 i2 o1 o2.
  Machine s1 i1 o1 /\ Machine s2 i2 o2 -> Machine (s1 /\ s2) (i1 \/ i2) (o1 /\ o2)
splice (m1 /\ m2) (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  ((o1 /\ t1) /\ (o2 /\ t2)) -> (o1 /\ o2) /\ either (\i1 -> t1 i1 /\ s2) (\i2 -> s1 /\ t2 i2)

demux :: ∀ s1 s2 i1 i2 o1 o2.
  Machine s1 i1 o1 /\ Machine s2 i2 o2 -> Machine (s1 \/ s2) (i1 \/ i2) (o1 \/ o2)
demux (m1 /\ m2) = either
  (Step.mapS Left <<< ultraleft m1)
  (Step.mapS Right <<< ultraright m2)
