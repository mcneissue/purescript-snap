module Data.Exists.Extras where

import Prelude

import Data.Exists (Exists, mkExists, runExists)

fill :: forall f a. Functor f => a -> Exists f -> f a
fill a = runExists (map (const a))

hoistExists :: forall f g. (f ~> g) -> Exists f -> Exists g
hoistExists nat = runExists (mkExists <<< nat)
