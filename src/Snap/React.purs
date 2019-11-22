module Snap.React where

import Prelude

import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Snap (Snapper, Target(..))
import Web.DOM (Element)
import React.Basic.DOM (render) as R
import React.Basic (JSX) as R

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
reactTarget e sync = Target go
  where
  go v = do
    liftEffect $ R.render v e
    _ <- liftAff $ AVar.take sync
    pure (Target go)
