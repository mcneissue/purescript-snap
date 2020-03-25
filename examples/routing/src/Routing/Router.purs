module Examples.Routing.Router where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', print, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

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

urlFor :: Route -> String
urlFor r = "/#" <> print parser r
