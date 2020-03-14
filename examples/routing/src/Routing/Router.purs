module Examples.Routing.Router where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Routing.Duplex (RouteDuplex', print, root)
import Routing.Duplex as RD
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError(..))
import Routing.Hash (matchesWith)

data Route 
  = Root 
  | CatTron
  | Reducer
  | TodoMvc
  | Transactional

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

parser :: RouteDuplex' Route
parser = root $ sum
  { "Root": noArgs
  , "CatTron": "cattron" / noArgs
  , "Reducer": "reducer" / noArgs
  , "TodoMvc": "todomvc" / noArgs
  , "Transactional": "transactional" / noArgs
  }

-- Create a hash-based router
mkRouter :: (Maybe Route -> Route -> Aff Unit) -> Aff (Effect Unit)
mkRouter f = liftEffect $ matchesWith parse handle
  where
  handle mr r = do
    logShow r
    launchAff_ $ f mr r

  parse s = case RD.parse parser s of
    Left EndOfPath -> Right Root
    Left e -> Left e
    r -> r

urlFor :: Route -> String
urlFor r = "/#" <> print parser r
