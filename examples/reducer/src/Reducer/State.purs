module Examples.Reducer.State where

import Prelude

import Data.Either (Either(..))
import Control.Monad.Cont (ContT(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Snap.Mealy as Mealy
import Control.K as K

type DUpdate = Mealy.FetchUpdate Void String
type DState = Mealy.FetchState Void String

data CounterAction
  = Increment
  | Decrement

type Action = CounterAction \/ DUpdate

type State = Int /\ DState

counter :: Mealy.EMachine Int CounterAction Unit
counter s = Mealy.mkTransition $ \u -> reducer u s
  where
  reducer Increment x = Mealy.Yes (x + 1) K.empty
  reducer Decrement x = Mealy.Yes (x - 1) K.empty

-- delayer :: Mealy.EMachine DState DUpdate
-- delayer s = Mealy.emptyCont /\ transition
--   where
--   task = case s of
--     Idle -> pure Load
--     Loading -> ContT \cb -> launchAff_ (delay (Milliseconds 1000.0) *> liftEffect (cb $ Succeed "Loaded a thing" ))
--     Success _ -> Mealy.emptyCont
--   transition u = case s of
--     Loading -> case u of
--       Succeed x -> Mealy.Yes (Success x) Mealy.emptyCont
--       _ -> Mealy.No
--     Success _ -> case u of
--       Load -> Mealy.Yes Loading Mealy.emptyCont
--       _ -> Mealy.No

delayer :: Mealy.EMachine (Mealy.FetchState Void String) (Mealy.FetchUpdate Void String) Unit
delayer = Mealy.fetchMachine $ \cb -> launchAff_ (delay (Milliseconds 1000.0) *> liftEffect (cb $ Right "Loaded a thing" ))

machine :: Mealy.EMachine State Action Unit
machine = Mealy.mapV (const unit) $ Mealy.esplice counter delayer

initialState :: State
initialState = 0 /\ Mealy.Idle

initialInputs :: Array Action
initialInputs = [ Right Mealy.Load, Left Increment ]
