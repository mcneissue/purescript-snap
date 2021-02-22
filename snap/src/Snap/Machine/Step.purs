module Snap.Machine.Step where

import Prelude hiding (pure, join)
import Data.Bifunctor (lmap)
import Snap.Machine.Transition (Transition)

type Step s i e = i -> Transition s e

mapS :: ∀ s s' i e. (s -> s') -> Step s i e -> Step s' i e
mapS f s i = lmap f $ s i
