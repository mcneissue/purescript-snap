module Examples.TransactionalForm.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor.Optics (Transactional)

type FormData = { name :: String, age :: Int }
type State = Transactional { name :: String, age :: Int }

initialState :: State
initialState =
  { modification: Nothing
  , value: { name: "", age: 0 }
  }
