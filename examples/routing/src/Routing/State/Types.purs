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

type State = PageState + ( nav :: RouteError \/ Route )
type Update = PageUpdate + ( nav :: Route )
