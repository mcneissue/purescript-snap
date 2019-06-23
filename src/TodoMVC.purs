module TodoMVC where

import Prelude hiding (map,apply)

import Data.Array (singleton)
import Data.Functor.Variant (SProxy(..))
import Data.Lens.Record (prop)
import Data.Lens.Record.Extra (extracted, remapped)
import Data.Profunctor (lcmap)
import Data.Profunctor.Optics (all, by, partsOf', traversed')
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (div, text) as R
import React.Basic.Events (handler_) as R
import Snap.Component (($!), (#!))
import Snap.React.Component as S
import Snap.SYTC.Component (Cmp', apply, map, switch, (<$>!))

σ :: forall s. SProxy s
σ = SProxy

-- The state corresponding to a todo item
type Todo = { done :: Boolean, hovered :: Boolean, editing :: Boolean, value :: String }

-- The editor for todo items
editor :: Cmp' Effect JSX Todo
editor = remapped scheme >>> extracted scheme $! S.input
  where
  scheme = { editing: focused, value }
  focused = SProxy :: _ "focused"
  value = SProxy :: _ "value"

-- The renderer for todo items
viewer :: Cmp' Effect JSX Todo
viewer = ado
  done   <- S.checkbox #! prop (σ :: _ "done")
  text   <- S.text #! prop (σ :: _ "value")
  toggle <- S.conditional #! lcmap (_.hovered)
  div    <- container
  in div [done {}, text, toggle $ R.text "hovering"]
  where
  container set s children =
    R.div
      { children
      , onClick: R.handler_ $ set $ s { editing = true }
      }

wrapInDiv :: JSX -> JSX
wrapInDiv = R.div <<< { children: _ } <<< singleton

-- A todo item
-- Depending on the value of the "editing" field, shows an
-- editor or a renderer
todo :: Cmp' Effect JSX Todo
todo = switch editor viewer #! by _.editing # map wrapInDiv

type Todos = Array Todo

-- A checkbox to control the state of all todo items
allDone :: Cmp' Effect JSX (Array Todo)
allDone = S.checkbox #! all >>> partsOf' (traversed' <<< prop (σ :: _ "done")) # map (_ $ {})

type App = Todos

-- The application consists of a bunch of todos, a
-- checkbox to control all of their "done" values,
-- and a text element showing the overall application
-- state
app :: Cmp' Effect JSX App
app = todos <> (wrapInDiv <$>! allDone) <> (wrapInDiv <$>! debug)
  where
  todos = todo #! traversed'
  debug = S.text #! lcmap show
