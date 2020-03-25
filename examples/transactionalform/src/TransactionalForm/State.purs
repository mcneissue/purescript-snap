module Examples.TransactionalForm.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor.Optics (Transactional)
import Effect.AVar (AVar)
import Effect.Aff.Class (class MonadAff)
import Snap (Snapper')
import Snap.React (affSnapper_)

type FormData = { name :: String, age :: Int }
type State = Transactional { name :: String, age :: Int }

initialState :: State
initialState =
  { modification: Nothing
  , value: { name: "", age: 0 }
  }

snapper :: forall m. MonadAff m => AVar Unit -> m (Snapper' m State)
snapper = affSnapper_ initialState
