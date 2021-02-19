module Snap.Machine (module Step, module Export, module Snap.Machine) where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Monoidal (class Monoidal, class Semigroupal, class Unital)
import Data.Profunctor.Traverse (class BiInvariant, biinvmap)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Snap.Machine.SYTC (EMachine, mapE, mapS, mapI) as Export
import Snap.Machine.SYTC as SYTC
import Snap.Machine.Step as Step

newtype Machine e s i = Machine (SYTC.Machine s i e)

derive instance newtypeMachine :: Newtype (Machine e s i) _

instance biinvariantMachine :: BiInvariant (Machine e) where
  biinvmap f g _ h (Machine m) = Machine $ SYTC.mapS f g $ SYTC.mapI h $ m

instance tetSemigroupalMachine :: Semigroupal (->) Tuple Either Tuple (Machine e) where
  pzip (Machine m1 /\ Machine m2) = Machine $ SYTC.splice m1 m2

instance tetUnitalMachine :: Unital (->) Unit Void Unit (Machine e) where
  punit = const $ Machine SYTC.unit

instance tetMonoidalMachine :: Monoidal (->) Tuple Unit Either Void Tuple Unit (Machine e)

newtype Feedback m s i = Feedback (SYTC.Machine s i (m i))

derive instance newtypeFeedbackLoop :: Newtype (Feedback m s i) _

instance biinvariantFeedback :: Functor m => BiInvariant (Feedback m) where
  biinvmap f g h i (Feedback m) = Feedback $ SYTC.mapE (map h) $ unwrap $ biinvmap f g h i (Machine m)

instance tetSemigroupalFeedback :: Functor m => Semigroupal (->) Tuple Either Tuple (Feedback m) where
  pzip (Feedback m1 /\ Feedback m2) = Feedback $ SYTC.splice (SYTC.mapE (map Left) m1) (SYTC.mapE (map Right) m2)

instance tetUnitalFeedback :: Unital (->) Unit Void Unit (Feedback m) where
  punit = const $ Feedback SYTC.unit

instance tetMonoidalFeedback :: Functor m => Monoidal (->) Tuple Unit Either Void Tuple Unit (Feedback m)
