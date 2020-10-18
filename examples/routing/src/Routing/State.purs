module Examples.Routing.State (module Examples.Routing.State, module ST) where

import Prelude

import Data.Profunctor.Monoidal (mono)
import Data.Profunctor.Traverse (sequenceSplice)
import Data.Variant (Variant)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Examples.CatTron.State as CatTron
import Examples.Reducer.State as Reducer
import Examples.Routing.Router (parser)
import Examples.Routing.State.Types (Update, State)
import Examples.Routing.State.Types as ST
import Examples.TodoMVC.State as TodoMvc
import Examples.TransactionalForm.State as Transactional
import Snap (Snapper, hoist)
import Snap.React (route)

snapper :: AVar Unit -> Aff (Snapper Aff (Variant Update) (Record State))
snapper av = ado
  nav      <- pure $ hoist liftEffect $ route parser
  root     <- pure $ mono
  todomvc  <- TodoMvc.snapper av
  cattron  <- CatTron.snapper av
  transact <- Transactional.snapper av
  reducer  <- Reducer.snapper av
  in
  sequenceSplice { nav, root, todomvc, cattron, transact, reducer }
