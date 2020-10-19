module Snap.React.Target where

import Prelude

import Data.Profunctor (lcmap)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import React.Basic (JSX) as R
import React.Basic.DOM (render) as R
import Data.Machine.Mealy (MealyT, Step(..), mealy)
import Web.DOM (Element)

reactTarget :: forall m b. MonadAff m => Element -> AVar b -> MealyT m R.JSX b
reactTarget e = lcmap pure <<< reactTargetM e

reactTargetM :: forall m b. MonadAff m => Element -> AVar b -> MealyT m (m R.JSX) b
reactTargetM e sync = mealy go
  where
  go v = do
    v' <- v
    liftEffect $ R.render v' e
    b <- liftAff $ AVar.take sync
    pure $ Emit b (mealy go)
