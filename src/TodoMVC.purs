module TodoMVC where

import Prelude hiding (map,apply)

import Data.Array (filter, length, replicate, snoc)
import Data.Eq (class Eq)
import Data.Functor.Variant (SProxy(..))
import Data.Lens (Lens', _Just)
import Data.Lens.Record (prop)
import Data.Lens.Record.Extra (extractedBy, remappedBy)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (lcmap)
import Data.Profunctor.Optics (all, by, partsOf', traversed', withered', overArray, countBy)
import Data.String (trim)
import Debug.Trace (spy)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_) as R
import Snap.Component (($!), (#!))
import Snap.React.Component (InputState, (|-), (|<), (|=))
import Snap.React.Component as S
import Snap.SYTC.Component (Cmp, Cmp', (<#>!), (<$>!), (>>=!))
import Snap.SYTC.Component as C
import Unsafe.Coerce (unsafeCoerce)

_done    = SProxy :: _ "done"
_hovered = SProxy :: _ "hovered"
_editing = SProxy :: _ "editing"
_value   = SProxy :: _ "value"
_focused = SProxy :: _ "focused"
_newTodo = SProxy :: _ "newTodo"
_todos   = SProxy :: _ "todos"
_filter  = SProxy :: _ "filter"

-- #### STATE

-- The state corresponding to a todo item
type Todo =
  { done :: Boolean
  , hovered :: Boolean
  , editing :: Boolean
  , value :: String
  }

type Todos = Array Todo

data Filter = All | Active | Completed
derive instance eqFilter :: Eq Filter

instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

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
  , editing: false
  }

defaultNewTodo :: InputState
defaultNewTodo = { focused: true, value: "" }

-- Initial application state consists of three components
initialState :: App
initialState = { newTodo: defaultNewTodo, todos: [], filter: All }

-- #### UI

-- TODO: Handle escape button press
-- TODO: Destroy input if value is empty
-- The editor for todo items
editor :: Cmp' Effect JSX Todo
editor = subpart $! C.ado
  kp    <- S.keypressability
           # C.handle keyHandler
           #! prop _focused
  input <- S.input
  in kp input { className: "edit" }
  where
  subpart = extractedBy scheme <<< remappedBy scheme
  scheme = { editing: _focused, value: _value }
  keyHandler k | k == "Enter" = const false
               | otherwise    = identity

-- The renderer for todo items. Accepts some conditionally
-- rendered content that will be shown when hovering the todo
viewer :: Cmp' Effect (JSX -> JSX) Todo
viewer = C.ado
  chk  <- S.checkbox     #! prop _done
  txt  <- S.text         #! prop _value
  veil <- S.conditional  #! lcmap _.hovered
  ckbl <- S.clickability #  C.handle_ \s -> s { editing = true, hovered = true }
  hvbl <- S.hoverability #! prop _hovered
  in
  \extra ->
    hvbl R.div
    |= { className: "view" }
    |< [ chk { className: "toggle" }
       , ckbl R.label |- txt
       , veil extra
       ]

-- A todo item
-- Depending on the value of the "editing" field, shows an
-- editor or a renderer.
todo :: Cmp' Effect JSX (Maybe Todo)
todo = C.ado
  ev  <- editview #! _Just
  del <- S.button #  C.handle_ (const Nothing)
  in ev $ del { className: "destroy" }
  where
  editor' = const <$>! editor
  editview = C.switch editor' viewer #! by _.editing

shouldHide :: Filter -> Todo -> Boolean
shouldHide All       = const false
shouldHide Active    = _.done
shouldHide Completed = not _.done

listItem :: forall u u'. Cmp Effect (JSX -> Cmp Effect JSX (Maybe Todo) u') Filter u
listItem _ f v _ Nothing  = mempty
listItem _ f v _ (Just t) = R.li |= { className } |- v
  where
  className =
       (if t.editing        then " editing "   else "")
    <> (if t.done           then " completed " else "")
    <> (if (shouldHide f t) then " hidden "    else "")

-- A list of todos, which can delete themselves from the list
todos :: Cmp' Effect JSX App
todos = C.do
  li  <- listItem       #! prop _filter
  tds <- (todo >>=! li) #! prop _todos <<< withered'
  C.pure $ R.ul
     |= { className: "todo-list" }
     |- tds

-- A checkbox to control the state of all todo items
allDone :: Cmp' Effect JSX Todos
allDone = C.ado
  chk <- S.checkbox #! all >>> partsOf' (traversed' <<< prop _done)
  in chk { id: "toggle-all", className: "toggle-all" }
     <> R.label
        |= { htmlFor: "toggle-all" }
        |- R.text "Mark all as complete"

-- The header for the todo list
header :: Cmp' Effect JSX App
header = C.ado
  key <- S.keypressability
         # C.handle \k -> if k == "Enter"
                          then addTodo
                          else identity
  inp <- S.input #! prop _newTodo
  in R.header
     |= { className: "header" }
     |< [ R.h1 |- R.text "todos"
        , key inp { className: "new-todo", placeholder: "What needs to be done?" }
        ]
  where
  addTodo s =
    let v = trim s.newTodo.value
    in if v == ""
       then s
       else s { todos = s.todos `snoc` createTodo v, newTodo = defaultNewTodo }

itemCount :: forall u. Cmp Effect JSX Int u
itemCount _ = go
  where
  wrap n s = R.strong |- R.text n <> R.text s
  go 0 = wrap "no" " items left"
  go 1 = wrap "one" " item left"
  go n = wrap (show n) " items left"

url :: Filter -> String
url All       = "#/"
url Active    = "#/active"
url Completed = "#/completed"

anchor :: forall u. Cmp Effect (Filter -> JSX) Filter u
anchor _ s f = a |= { href: url f } |- R.text (show f)
  where
  a | s == f    = R.a |= { className: "selected" }
    | otherwise = R.a

filters :: Cmp' Effect JSX Filter
filters = C.ado
  a   <- anchor
  all <- S.clickability # C.handle_ (const All)
  act <- S.clickability # C.handle_ (const Active)
  com <- S.clickability # C.handle_ (const Completed)
  in R.ul
     |= { className: "filters" }
     |< [ all R.li |- a All
        , act R.li |- a Active
        , com R.li |- a Completed
        ]

footer :: Cmp' Effect JSX App
footer = C.ado
  count <- itemCount     #! prop _todos <<< countBy (not _.done)
  veil  <- S.conditional #! prop _todos <<< countBy _.done <<< lcmap (_ > 0)
  fltrs <- filters       #! prop _filter
  clear <- S.button
           #  C.handle_ (_ <#> const false)
           #! prop _todos <<< overArray (prop _done)
  in R.footer
     |= { className: "footer" }
     |< [ R.span
          |= { className: "todo-count" }
          |- count
        , fltrs
        , veil
          $ clear
            |= { className: "clear-completed" }
            |- R.text "Clear completed"
        ]

-- The overall application
app :: Cmp' Effect JSX App
app = C.ado
  veil <- S.conditional
          #! prop _todos <<<
             countBy (const true) <<<
             lcmap (_ > 0)
  hdr  <- header
  tds  <- todos
  tgl  <- allDone #! prop _todos
  ftr  <- footer
  dbg  <- S.debug
  in R.section
     |= { className: "todoapp" }
     |< [ hdr
        , veil $ R.section
          |= { className: "main" }
          |< [ tgl
             , tds
             ]
        , veil $ ftr
        ]
     <> dbg
