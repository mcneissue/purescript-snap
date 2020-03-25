module Examples.Routing.State where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Profunctor.Monoidal (mono, (|&))
import Data.Tuple.Nested (type (/\))
import Effect.AVar (AVar)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Examples.CatTron.State as CatTron
import Examples.Reducer.State as Reducer
import Examples.Routing.Router (parser)
import Examples.Routing.Router as Router
import Examples.TodoMVC.State as TodoMvc
import Examples.TransactionalForm.State as Transactional
import Routing.Duplex.Parser (RouteError)
import Snap (Snapper, hoist)
import Snap.React (route)

snapper :: forall m. MonadAff m =>
  AVar Unit ->
  m (Snapper m
    (Router.Route                 \/ Void \/ TodoMvc.App \/ CatTron.State \/ Transactional.State \/ Reducer.Action)
    ((RouteError \/ Router.Route) /\ Unit /\ TodoMvc.App /\ CatTron.State /\ Transactional.State /\ Reducer.State)
    )
snapper av = ado
  nav <- pure $ hoist liftEffect $ route parser
  rut <- pure $ mono
  tmv <- TodoMvc.snapper av
  cat <- CatTron.snapper av
  trn <- Transactional.snapper av
  red <- Reducer.snapper av
  in
  nav |& rut |& tmv |& cat |& trn |& red
