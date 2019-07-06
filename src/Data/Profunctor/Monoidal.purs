module Data.Profunctor.Monoidal where

import Prelude

import Data.Either (Either)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple)

-- Profunctors that preserve the cartesian monoidal structure on Hask to various extents
class (Strong p, Choice p) <= SemigroupalMono p where
  zipMono :: forall a b. p a a -> p b b -> p (Tuple a b) (Tuple a b)

class SemigroupalMono p <= Semigroupal p where
  zip :: forall a b c d. p a b -> p c d -> p (Tuple a c) (Tuple b d)

class SemigroupalMono p <= MonoidalMono p where
  infinite :: p Unit Unit

class (Semigroupal p, SemigroupalMono p) <= Monoidal p

-- Profunctors that preserve the cocartesian monoidal structure on Hask to various extents
-- TODO: Determine whether this should depend on anything beyond Profunctor
class Profunctor p <= CosemigroupalMono p where
  switchMono :: forall a b. p a a -> p b b -> p (Either a b) (Either a b)

class CosemigroupalMono p <= Cosemigroupal p where
  switch :: forall a b c d. p a b -> p c d -> p (Either a c) (Either b d)

class CosemigroupalMono p <= ComonoidalMono p where
  never :: p Void Void

class (Cosemigroupal p, ComonoidalMono p) <= Comonoidal p
