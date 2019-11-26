module Examples.Reducer.State where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), delay, forkAff, Fiber, Aff)

data DelayerAction
  = Load (DelayerAction -> Aff Unit)
  | Loaded String

data CounterAction
  = Increment
  | Decrement

type Action = Either CounterAction DelayerAction

type State =
  { counter :: Int
  , delayer :: Maybe String
  }

initialState :: State
initialState = 
  { counter: 0
  , delayer: Just "Click the button to launch a delayed request."
  }

-- TODO: Lenses and Variant
rootReducer :: Action -> State -> Aff State
rootReducer act s = case act of
  Left ca  -> pure $ s { counter = counterReducer ca s.counter }
  Right da -> (\ds -> s { delayer = ds }) <$> delayerReducer da s.delayer

counterReducer :: CounterAction -> Int -> Int
counterReducer Increment x = x + 1
counterReducer Decrement x = x - 1

delayerReducer :: DelayerAction -> Maybe String -> Aff (Maybe String)
delayerReducer (Load put) _ = do
  _ <- affDelay 1000.0 $ put (Loaded "Delayed request completed.")
  pure Nothing
delayerReducer (Loaded s) _ = pure $ Just s

affDelay :: forall a. Number -> Aff a -> Aff (Fiber a)
affDelay t a = forkAff $ delay (Milliseconds t) *> a