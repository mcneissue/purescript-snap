module TodoMVC.UI where

import Prelude hiding (map,apply)

import Data.Array (snoc)
import Data.Lens (_Just)
import Data.Lens as L
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor as P
import Data.Profunctor.Optics (all, by, countBy, edited, overArray, partsOf', traversed', withered')
import Data.String (trim)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (a, div, footer, h1, header, label, li, section, span, strong, text, ul, input) as R
import Snap.Component ((#!))
import Snap.React.Component ((|-), (|<), (|=), (|~))
import Snap.React.Component as S
import Snap.SYTC.Component (Cmp, Cmp', (<$>!), (<*>!))
import Snap.SYTC.Component as C
import TodoMVC.State (App, Filter(..), Todo, Todos, _done, _hovered, _value, _newTodo, _todos, _filter, createTodo, defaultNewTodo, editingTodo, shouldHide, todoValue)

-- #### UI

-- TODO: Destroy input if value is empty
-- The editor for todo items
editor :: Cmp' Effect JSX Todo
editor = C.ado
  editing <- S.transactionality
             { change: S.editability
             , save  : S.enter
             , revert: S.escape
             }
             #! edited >>> todoValue
  focus <- S.focusability #! editingTodo
  in R.input |~ editing |~ focus $ { className: "edit" }

-- The renderer for todo items. Accepts some conditionally
-- rendered content that will be shown when hovering the todo
viewer :: Cmp' Effect (JSX -> JSX) Todo
viewer = C.ado
  chk  <- S.checkbox     #! _done
  txt  <- S.text         #! _value
  veil <- S.conditional  #! P.lcmap _.hovered
  ckbl <- S.clickability #! P.rmap (const true) >>> editingTodo
  hvbl <- S.hoverability #! _hovered
  in
  \extra ->
    R.div
    |~ hvbl
    |= { className: "view" }
    |< [ chk { className: "toggle" }
       , R.label |~ ckbl |- txt
       , veil extra
       ]

-- A todo item
-- Depending on the value of the "edit" field, shows an
-- editor or a renderer.
todo :: Cmp' Effect JSX (Maybe Todo)
todo = C.ado
  ev  <- editview #! _Just
  del <- S.button #  C.handle_ (const Nothing)
  in ev $ del { className: "destroy" }
  where
  editor' = const <$>! editor
  editview = C.switch editor' viewer #! by (L.view editingTodo)

listItem :: forall u u'. Cmp Effect (Cmp Effect (JSX -> JSX) (Maybe Todo) u') Filter u
listItem _ _ _ Nothing  _ = mempty
listItem _ f _ (Just t) v = R.li |= { className } |- v
  where
  className =
       (if isJust t.edit    then " editing "   else "")
    <> (if t.done           then " completed " else "")
    <> (if (shouldHide f t) then " hidden "    else "")

-- A list of todos, which can delete themselves from the list
todos :: Cmp' Effect JSX App
todos = C.do
  li  <- listItem       #! _filter
  tds <- (li <*>! todo) #! _todos <<< withered'
  C.pure $ R.ul
     |= { className: "todo-list" }
     |- tds

-- A checkbox to control the state of all todo items
allDone :: Cmp' Effect JSX Todos
allDone = C.ado
  chk <- S.checkbox #! all >>> partsOf' (traversed' <<< _done)
  in chk { id: "toggle-all", className: "toggle-all" }
     <> R.label
        |= { htmlFor: "toggle-all" }
        |- R.text "Mark all as complete"

-- The header for the todo list
header :: Cmp' Effect JSX App
header = C.ado
  key <- S.enter # C.handle_ addTodo
  inp <- S.input #! _newTodo
  in R.header
     |= { className: "header" }
     |< [ R.h1 |- R.text "todos"
        , inp |~ key $ { className: "new-todo", placeholder: "What needs to be done?" }
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
     |< [ R.li |~ all |- a All
        , R.li |~ act |- a Active
        , R.li |~ com |- a Completed
        ]

footer :: Cmp' Effect JSX App
footer = C.ado
  count <- itemCount     #! _todos <<< countBy (not _.done)
  veil  <- S.conditional #! _todos <<< countBy _.done <<< P.lcmap (_ > 0)
  fltrs <- filters       #! _filter
  clear <- S.button
           #  C.handle_ (_ <#> const false)
           #! _todos <<< overArray _done
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
          #! _todos <<<
             countBy (const true) <<<
             P.lcmap (_ > 0)
  hdr  <- header
  tds  <- todos
  tgl  <- allDone #! _todos
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
