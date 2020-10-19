module Snap (module Snap, module C, module S) where

import Prelude


import Data.Machine.Mealy (MealyT, Step(..), runMealyT)

import Snap.Component as C
import Snap.Snapper as S

import Snap.Component.SYTC (Component)
import Snap.Snapper (Snapper(..))

encapsulate :: forall m v b s u x y. Functor m => Snapper m b u s -> Component m v b s u -> Component m (m v) b x y
encapsulate (Snapper { get, put }) cmp _ _ = get <#> cmp put

snap' :: forall m v b x
       . Monad m
      => Component m v b Unit Void
      -> MealyT m v b
      -> (b -> m x)
      -> m Unit
snap' cmp t f = loop t
  where
  v = cmp absurd unit
  loop mealy = do
    t' <- runMealyT mealy v
    case t' of
      Halt        -> pure unit
      Emit b next -> f b *> loop next

snap :: forall m v b
      . Monad m
     => Component m v b Unit Void
     -> MealyT m v b
     -> m Unit
snap c m = snap' c m (const $ pure unit)
