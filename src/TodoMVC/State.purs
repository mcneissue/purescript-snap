module TodoMVC.State where

import Prelude

import Data.Lens as L
import Data.Lens.Record (prop)
import Data.Lens.Record.Extra (extractedBy, remappedBy)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Optics (Editable, isEditing)
import Data.Symbol (SProxy(..))
import Snap.React.Component (InputState)

-- TODO:
-- 1. Set up routing stuff

-- #### STATE

-- The state corresponding to a todo item
type Todo =
  { done :: Boolean
  , hovered :: Boolean
  , value :: String
  , edit :: Maybe String
  }

type Todos = Array Todo

data Filter = All | Active | Completed
derive instance eqFilter :: Eq Filter

instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

shouldHide :: Filter -> Todo -> Boolean
shouldHide All       = const false
shouldHide Active    = _.done
shouldHide Completed = not _.done

type App =
  { newTodo :: InputState
  , todos :: Todos
  , filter :: Filter
  }

-- Create a new todo
createTodo :: String -> Todo
createTodo =
  { value: _
  , done: false
  , hovered: false
  , edit: Nothing
  }

defaultNewTodo :: InputState
defaultNewTodo = { focused: true, value: "" }

-- Initial application state consists of three components
initialState :: App
initialState = { newTodo: defaultNewTodo, todos: [], filter: All }

todoValue :: L.Lens' Todo (Editable String)
todoValue = remappedBy scheme >>> extractedBy scheme
  where
  scheme = { value: SProxy :: _ "saved", edit: SProxy :: _ "modified" }

editingTodo :: L.Lens' Todo Boolean
editingTodo = isEditing >>> todoValue

proxies =
  { done:    SProxy :: _ "done"
  , hovered: SProxy :: _ "hovered"
  , edit:    SProxy :: _ "edit"
  , value:   SProxy :: _ "value"
  , focused: SProxy :: _ "focused"
  , newTodo: SProxy :: _ "newTodo"
  , todos:   SProxy :: _ "todos"
  , filter:  SProxy :: _ "filter"
  }

_done    = prop proxies.done
_hovered = prop proxies.hovered
_edit    = prop proxies.edit
_value   = prop proxies.value
_focused = prop proxies.focused
_newTodo = prop proxies.newTodo
_todos   = prop proxies.todos
_filter  = prop proxies.filter
