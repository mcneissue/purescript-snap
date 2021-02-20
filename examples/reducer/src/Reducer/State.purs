module Examples.Reducer.State where

import Prelude

import Control.K as K
import Control.Monad.Cont (ContT(..), runContT)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Newtype (unwrap)
import Data.Profunctor.Traverse (sequenceSplice, sequenceSwitch)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Snap.Machine (Machine(..))
import Snap.Machine as Machine
import Snap.Machine.Fetch as Fetch
import Snap.Machine.Step (Transition(..))

type DUpdate = Fetch.FetchUpdate Void String
type DState = Fetch.FetchState Void String

data CounterAction
  = Increment
  | Decrement

type Action = Variant (counter :: CounterAction, delayer :: DUpdate)

type State = { counter :: Int, delayer :: DState }

counter :: Machine.EMachine Int CounterAction
counter s u = reducer u s
  where
  reducer Increment x = Yes (x + 1) K.empty
  reducer Decrement x = Yes (x - 1) K.empty

delayer :: Machine.EMachine (Fetch.FetchState Void String) (Fetch.FetchUpdate Void String)
delayer = Fetch.fetchMachine $ \cb -> launchAff_ (delay (Milliseconds 1000.0) *> liftEffect (cb $ Right "Loaded a thing" ))

machine :: Machine.EMachine State Action
machine = Machine.mapE runContT $ unwrap $ sequenceSwitch
 { counter: Machine.Feedback $ Machine.mapE ContT counter
 , delayer: Machine.Feedback $ Machine.mapE ContT delayer
 }

initialState :: State
initialState = { counter: 0, delayer: Fetch.Idle }

initialInputs :: Array Action
initialInputs = [ inj (SProxy :: _ "delayer") Fetch.Load, inj (SProxy :: _ "counter") Increment ]
