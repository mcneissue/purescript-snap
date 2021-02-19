module Examples.Reducer.State where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Snap.Machine as Machine
import Snap.Machine.Fetch as Fetch
import Control.K as K

type DUpdate = Fetch.FetchUpdate Void String
type DState = Fetch.FetchState Void String

data CounterAction
  = Increment
  | Decrement

type Action = CounterAction \/ DUpdate

type State = Int /\ DState

counter :: Machine.EMachine Int CounterAction
counter s u = reducer u s
  where
  reducer Increment x = Machine.Yes (x + 1) K.empty
  reducer Decrement x = Machine.Yes (x - 1) K.empty

delayer :: Machine.EMachine (Fetch.FetchState Void String) (Fetch.FetchUpdate Void String)
delayer = Fetch.fetchMachine $ \cb -> launchAff_ (delay (Milliseconds 1000.0) *> liftEffect (cb $ Right "Loaded a thing" ))

machine :: Machine.EMachine State Action
machine = Machine.esplice counter delayer

initialState :: State
initialState = 0 /\ Fetch.Idle

initialInputs :: Array Action
initialInputs = [ Right Fetch.Load, Left Increment ]
