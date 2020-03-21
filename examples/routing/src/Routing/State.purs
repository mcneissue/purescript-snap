module Examples.Routing.State where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
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

derive instance genericRouteState :: Generic RouteState _

instance showRouteState :: Show RouteState where
  show = genericShow

type State = RouteState

data Action
  = Navigate Router.Route
  | Update RouteState -- Handle updates for non-reducer components
  | ReducerAction Reducer.Action -- Handle updates for the Reducers examples

initialState :: State
initialState = Root

stateFor :: Router.Route -> State
stateFor r = case r of
  Router.Root -> Root
  Router.CatTron -> CatTron CatTron.initialState
  Router.Reducer -> Reducer Reducer.initialState
  Router.TodoMvc -> TodoMvc TodoMvc.initialState
  Router.Transactional -> Transactional Transactional.initialState

reducer :: Action -> State -> Aff State
reducer a s = case a of
  Navigate r -> do
    log $ "Navigating to " <> show r
    pure $ stateFor r
  Update s' -> pure s'
  ReducerAction ra ->
    let rs = case s of
               Reducer x -> x
               _ -> Reducer.initialState
    in Reducer <$> Reducer.rootReducer ra rs
