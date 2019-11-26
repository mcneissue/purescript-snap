module Examples.Routing.Router where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', parse, print, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (matchesWith)

data Route 
  = Root 
  | CatTron
  | Reducer
  | TodoMvc
  | Transactional

derive instance genericRoute :: Generic Route _

parser :: RouteDuplex' Route
parser = root $ sum
  { "Root": noArgs
  , "CatTron": "cattron" / noArgs
  , "Reducer": "reducer" / noArgs
  , "TodoMvc": "todomvc" / noArgs
  , "Transactional": "transactional" / noArgs
  }

-- Create a hash-based router
mkRouter :: (Maybe Route -> Route -> Effect Unit) -> Effect (Effect Unit)
mkRouter = matchesWith (parse parser)

urlFor :: Route -> String
urlFor r = "/#" <> print parser r