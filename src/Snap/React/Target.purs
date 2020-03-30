module Snap.React.Target where

import Prelude

import Data.Functor.Contravariant (cmap)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import React.Basic (JSX) as R
import React.Basic.DOM (render) as R
import Snap (Target(..))
import Web.DOM (Element)

reactTarget :: forall m. MonadAff m => Element -> AVar Unit -> Target m R.JSX
reactTarget e = cmap pure <<< reactTargetM e

reactTargetM :: forall m. MonadAff m => Element -> AVar Unit -> Target m (m R.JSX)
reactTargetM e sync = Target go
  where
  go v = do
    v' <- v
    liftEffect $ R.render v' e
    _ <- liftAff $ AVar.take sync
    pure $ Target go
