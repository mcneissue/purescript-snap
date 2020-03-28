module Examples.Routing.State (module Examples.Routing.State, module ST) where

import Prelude

import Data.Profunctor.Monoidal (mono)
import Data.Profunctor.Traverse (foldSplice)
import Data.Variant (Variant)
import Effect.AVar (AVar)
import Effect.Aff.Class (class MonadAff)
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

snapper :: forall m. MonadAff m => AVar Unit -> m (Snapper m (Variant Update) (Record State))
snapper av = ado
  nav      <- pure $ hoist liftEffect $ route parser
  root     <- pure $ mono
  todomvc  <- TodoMvc.snapper av
  cattron  <- CatTron.snapper av
  transact <- Transactional.snapper av
  reducer  <- Reducer.snapper av
  in
  foldSplice { nav, root, todomvc, cattron, transact, reducer }
