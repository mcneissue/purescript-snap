module Examples.Routing.Router where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso', re, (^.))
import Data.Profunctor (dimap)
import Data.Record.Choose (getField, hasField)
import Data.Symbol (SProxy(..))
import Examples.Routing.State.Types (Route)
import Partial.Unsafe (unsafeCrashWith)
import Routing.Duplex (RouteDuplex', print, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route'
  = Root
  | CatTron
  | Reducer
  | TodoMvc
  | Transactional

derive instance genericRoute :: Generic Route' _
derive instance eqRoute :: Eq Route'

instance showRoute :: Show Route' where
  show = genericShow

convert :: Iso' Route Route'
convert = dimap (bwd <<< getField) fwd
  where
  fwd Root          = hasField (SProxy :: _ "root")
  fwd CatTron       = hasField (SProxy :: _ "cattron")
  fwd Reducer       = hasField (SProxy :: _ "reducer")
  fwd TodoMvc       = hasField (SProxy :: _ "todomvc")
  fwd Transactional = hasField (SProxy :: _ "transact")

  bwd "root" = Root
  bwd "cattron" = CatTron
  bwd "reducer" = Reducer
  bwd "todomvc" = TodoMvc
  bwd "transact" = Transactional
  bwd x = unsafeCrashWith x

parser :: RouteDuplex' Route
parser = convert $ root $ sum
  { "Root": noArgs
  , "CatTron": "cattron" / noArgs
  , "Reducer": "reducer" / noArgs
  , "TodoMvc": "todomvc" / noArgs
  , "Transactional": "transactional" / noArgs
  }

urlFor :: Route' -> String
urlFor r = "/#" <> print parser (r ^. re convert)
