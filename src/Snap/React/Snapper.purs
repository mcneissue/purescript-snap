module Snap.React.Snapper where

import Prelude

import Data.Either (Either)
import Data.Profunctor (dimap)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routing.Duplex (RouteDuplex, parse, print)
import Routing.Duplex.Parser (RouteError)
import Routing.Hash (getHash, setHash)
import Snap.Snapper (Snapper(..), Snapper')

refSnapper :: forall s m. MonadAff m => Ref s -> AVar Unit -> Snapper m s s
refSnapper ref sync = Snapper { get, put }
  where
  get = liftEffect $ Ref.read ref
  put u = do
    liftEffect $ Ref.write u ref
    _ <- liftAff $ AVar.put unit sync
    pure unit

url :: Snapper' Effect String
url = Snapper { get: getHash, put: setHash }

route :: forall i o. RouteDuplex i o -> Snapper Effect i (Either RouteError o)
route r = dimap (print r) (parse r) $ url