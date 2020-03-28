module Snap.React.Snapper where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Profunctor (dimap)
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routing.Duplex (RouteDuplex, parse, print)
import Routing.Duplex.Parser (RouteError)
import Routing.Hash (getHash, setHash)
import Snap (reduced)
import Snap.Snapper (Snapper(..), Snapper')
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage

refSnapper :: forall s m.
  MonadAff m =>
  Ref s -> AVar Unit -> Snapper m s s
refSnapper ref sync = Snapper { get, put }
  where
  get = liftEffect $ Ref.read ref
  put u = do
    liftEffect $ Ref.write u ref
    _ <- liftAff $ AVar.put unit sync
    pure unit

affSnapper :: forall m u s.
  MonadAff m =>
  (u -> s -> m s) -> s -> AVar Unit -> m (Snapper m u s)
affSnapper red s sync = do
  r <- liftEffect $ Ref.new s
  pure $ reduced red $ refSnapper r sync

affSnapper_ :: forall m s.
  MonadAff m =>
  s -> AVar Unit -> m (Snapper m s s)
affSnapper_ = affSnapper $ \s _ -> pure s

url :: forall m.
  MonadEffect m =>
  Snapper' m String
url = Snapper { get: liftEffect $ getHash, put: liftEffect <<< setHash }

route :: forall m i o.
  MonadEffect m =>
  RouteDuplex i o -> Snapper m i (Either RouteError o)
route r = dimap (print r) (parse r) $ url

localstorage :: forall m.
  MonadEffect m =>
  String -> m (Snapper m String (Maybe String))
localstorage k = do
  w <- liftEffect $ window
  s <- liftEffect $ localStorage w
  pure $ Snapper { get: liftEffect $ Storage.getItem k s, put: \v -> liftEffect $ Storage.setItem k v s }
