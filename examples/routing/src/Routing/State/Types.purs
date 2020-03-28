module Examples.Routing.State.Types where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Record.Choose (HasField)
import Examples.CatTron.State as CatTron
import Examples.Reducer.State as Reducer
import Examples.TodoMVC.State as TodoMvc
import Examples.TransactionalForm.State as Transactional
import Routing.Duplex.Parser (RouteError)
import Type.Row (type (+))

type PageState r =
  ( root :: Unit
  , todomvc :: TodoMvc.App
  , cattron :: CatTron.State
  , transact :: Transactional.State
  , reducer :: Reducer.State
  | r
  )

type Route = HasField (PageState ())

type PageUpdate r =
  ( root :: Void
  , todomvc :: TodoMvc.App
  , cattron :: CatTron.State
  , transact :: Transactional.State
  , reducer :: Reducer.Action
  | r
  )

type NavState r = ( nav :: RouteError \/ Route | r )

type NavUpdate r = ( nav :: Route | r )

type State = PageState + NavState ()
type Update = PageUpdate + NavUpdate ()
