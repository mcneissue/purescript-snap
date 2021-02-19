module Examples.Reducer.State where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Snap.Mealy as Mealy
import Control.K as K

type DUpdate = Mealy.FetchUpdate Void String
type DState = Mealy.FetchState Void String

data CounterAction
  = Increment
  | Decrement

type Action = CounterAction \/ DUpdate

type State = Int /\ DState

counter :: Mealy.EMachine Int CounterAction
counter s u = reducer u s
  where
  reducer Increment x = Mealy.Yes (x + 1) K.empty
  reducer Decrement x = Mealy.Yes (x - 1) K.empty

delayer :: Mealy.EMachine (Mealy.FetchState Void String) (Mealy.FetchUpdate Void String)
delayer = Mealy.fetchMachine $ \cb -> launchAff_ (delay (Milliseconds 1000.0) *> liftEffect (cb $ Right "Loaded a thing" ))

machine :: Mealy.EMachine State Action
machine = Mealy.esplice counter delayer

initialState :: State
initialState = 0 /\ Mealy.Idle

initialInputs :: Array Action
initialInputs = [ Right Mealy.Load, Left Increment ]
