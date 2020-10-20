module Snap (module Snap, module C, module S) where

import Prelude

import Data.Machine.Mealy (MealyT, Step(..), runMealyT, mealy, stepMealy)

import Snap.Component as C
import Snap.Snapper as S

import Snap.Component.SYTC (Component)
import Snap.Snapper (Snapper(..))

encapsulate :: forall m v b s u x y z. Functor m => Snapper m b u s -> Component m v b s u -> Component m (m v) x y z
encapsulate (Snapper { get, put }) cmp _ _ = get <#> cmp put

rewrite :: forall m v b c. Monad m => (b -> m c) -> MealyT m v b -> MealyT m v c
rewrite f m = go m
  where
  go m = mealy \s -> runMealyT m s >>= case _ of
    Halt -> pure Halt
    Emit b next -> f b <#> \c -> Emit c (go next)

snap :: forall m v b
      . Monad m
     => Component m v Unit Unit Void
     -> MealyT m v Unit
     -> m Unit
snap cmp t = loop t
  where
  v = cmp absurd unit
  loop m = runMealyT m v >>= case _ of
    Emit _ next -> loop next
    Halt -> pure unit
