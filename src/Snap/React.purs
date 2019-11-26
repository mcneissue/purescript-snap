module Snap.React where

import Prelude

import Data.Functor.Contravariant (cmap)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Basic (JSX) as R
import React.Basic.DOM (render) as R
import Snap (Snapper, Target(..))
import Web.DOM (Element)

refSnapper :: forall s m. MonadAff m => Ref s -> AVar Unit -> Snapper m s s
refSnapper = refSnapper' (\s _ -> pure s)

refSnapper' :: forall s u m. MonadAff m => (u -> s -> m s) -> Ref s -> AVar Unit -> Snapper m s u
refSnapper' reducer ref sync = { get, put }
  where
  get = liftEffect $ Ref.read ref
  put u = do
    s  <- get
    s' <- reducer u s
    liftEffect $ Ref.write s' ref
    _ <- liftAff $ AVar.put unit sync
    pure unit

reactTarget :: forall m. MonadAff m => Element -> AVar Unit -> Target m R.JSX
reactTarget e = cmap pure <<< reactTargetM e

reactTargetM :: forall m. MonadAff m => Element -> AVar Unit -> Target m (m R.JSX)
reactTargetM e sync = Target go
  where
  go v = do
    v' <- v
    liftEffect $ R.render v' e
    _ <- liftAff $ AVar.take sync
    pure (Target go)