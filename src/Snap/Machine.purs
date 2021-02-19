module Snap.Machine (module Step, module SYTC, module Snap.Machine) where

import Prelude

import Data.Either (Either)
import Data.Profunctor.Monoidal (class Semigroupal, class Unital, class Monoidal)
import Data.Profunctor.Traverse (class BiInvariant)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Snap.Machine.SYTC as SYTC
import Snap.Machine.Step as Step

newtype Machine e s i = Machine (SYTC.Machine s i e)

instance biinvariantMachine :: BiInvariant (Machine e) where
  biinvmap f g _ h (Machine m) = Machine $ SYTC.mapS f g $ SYTC.mapI h $ m

instance tetSemigroupalMachine :: Semigroupal (->) Tuple Either Tuple (Machine e) where
  pzip (Machine m1 /\ Machine m2) = Machine $ SYTC.splice m1 m2

instance tetUnitalMachine :: Unital (->) Unit Void Unit (Machine e) where
  punit = const $ Machine SYTC.unit

instance tetMonoidalMachine :: Monoidal (->) Tuple Unit Either Void Tuple Unit (Machine e)
