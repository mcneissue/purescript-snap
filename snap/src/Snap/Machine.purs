module Snap.Machine (module Export, module Snap.Machine) where

import Prelude

import Data.Bifunctor.Invariant (class Invariant, invmap) as F2
import Data.Bifunctor.Monoidal (class Monoidal, class Semigroupal, class Unital)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, unwrap)
import Data.Trifunctor.Invariant (class Invariant, invmap) as F3
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Snap.Machine.SYTC (EMachine, mapE, mapS, mapI) as Export
import Snap.Machine.SYTC as SYTC

newtype Machine e s i = Machine (SYTC.Machine s i e)

derive instance newtypeMachine :: Newtype (Machine e s i) _

instance invariant2Machine :: F2.Invariant (Machine e) where
  invmap f g h i = F3.invmap identity identity f g h i

instance invariant3Machine :: F3.Invariant Machine where
  invmap f _ h i _ k (Machine m) = Machine $ SYTC.mapE f $ SYTC.mapS h i $ SYTC.mapI k $ m

instance tetSemigroupalMachine :: Semigroupal (->) Tuple Either Tuple (Machine e) where
  combine (Machine m1 /\ Machine m2) = Machine $ SYTC.mapE (either identity identity) $ SYTC.splice m1 m2

instance tetUnitalMachine :: Unital (->) Unit Void Unit (Machine e) where
  introduce = const $ Machine SYTC.unit

instance tetMonoidalMachine :: Monoidal (->) Tuple Unit Either Void Tuple Unit (Machine e)

newtype Feedback m s i = Feedback (SYTC.Machine s i (m i))

derive instance newtypeFeedbackLoop :: Newtype (Feedback m s i) _

instance invariant2Feedback :: Functor m => F2.Invariant (Feedback m) where
  invmap f g h i (Feedback m) = Feedback $ SYTC.mapE (map h) $ unwrap $ F2.invmap f g h i (Machine m)

instance tetSemigroupalFeedback :: Functor m => Semigroupal (->) Tuple Either Tuple (Feedback m) where
  combine (Feedback m1 /\ Feedback m2) = Feedback $ SYTC.mapE (either (map Left) (map Right)) $ SYTC.splice m1 m2

instance tetUnitalFeedback :: Unital (->) Unit Void Unit (Feedback m) where
  introduce = const $ Feedback SYTC.unit

instance tetMonoidalFeedback :: Functor m => Monoidal (->) Tuple Unit Either Void Tuple Unit (Feedback m)
