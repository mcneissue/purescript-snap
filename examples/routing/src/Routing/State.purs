module Examples.Routing.State where

import Prelude

import Examples.CatTron.State as CatTron
import Examples.Reducer.State as Reducer
import Examples.Routing.Router as Router
import Examples.TodoMVC.State as TodoMvc
import Examples.TransactionalForm.State as Transactional

data RouteState
  = Root
  | TodoMvc TodoMvc.App
  | CatTron CatTron.State
  | Reducer Reducer.State
  | Transactional Transactional.State

type State = RouteState

initialState :: State
initialState = Root

stateFor :: Router.Route -> State
stateFor r = case r of
  Router.Root -> Root
  Router.CatTron -> CatTron CatTron.initialState
  Router.Reducer -> Reducer Reducer.initialState
  Router.TodoMvc -> TodoMvc TodoMvc.initialState
  Router.Transactional -> Transactional Transactional.initialState