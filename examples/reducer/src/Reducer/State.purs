module Examples.Reducer.State where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Snap.Machine.FeedbackLoop (FeedbackLoop)
import Snap.Machine.FeedbackLoop as FeedbackLoop

data DUpdate = Load | Succeed String
data DState = Loading | Success String

derive instance genericDState :: Generic DState _

instance showDState :: Show DState where
  show = genericShow

data CounterAction
  = Increment
  | Decrement

type Action = CounterAction \/ DUpdate

type State = Int /\ DState

counter :: forall m. Applicative m => FeedbackLoop Unit m Int CounterAction
counter s = FeedbackLoop.emptyCont /\ \u -> reducer u s
  where
  reducer Increment x = x + 1
  reducer Decrement x = x - 1

delayer :: FeedbackLoop Unit Effect DState DUpdate
delayer s = task /\ transition
  where
  task = case s of
    Loading -> ContT \cb -> launchAff_ (delay (Milliseconds 1000.0) *> liftEffect (cb $ Succeed "Loaded a thing" ))
    Success _ -> FeedbackLoop.emptyCont
  transition u = case s of
    Loading -> case u of
      Load -> unsafeThrow "Invalid State Transition"
      Succeed x -> Success x
    Success _ -> case u of
      Load -> Loading
      Succeed _ -> unsafeThrow "Invalid State Transition"

machine :: FeedbackLoop Unit Effect State Action
machine = FeedbackLoop.splice counter delayer

initialState :: State
initialState = 0 /\ Success "Click the button to execute an async action."
