module Examples.Reducer.State where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Monoidal ((|&))
import Data.Tuple.Nested (type (/\))
import Effect.AVar (AVar)
import Effect.Aff (Milliseconds(..), delay, forkAff, Fiber, Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Snap (Snapper)
import Snap.React.Snapper (affSnapper)

data DelayerAction
  = Load (DelayerAction -> Aff Unit)
  | Loaded String

data CounterAction
  = Increment
  | Decrement

type Action = CounterAction \/ DelayerAction

type State = Int /\ Maybe String

counterSnapper :: forall m.
  MonadAff m =>
  AVar Unit -> m (Snapper m CounterAction Int)
counterSnapper = affSnapper reducer init
  where
  init = 0
  reducer Increment x = pure $ x + 1
  reducer Decrement x = pure $ x - 1

affDelay :: forall a. Number -> Aff a -> Aff (Fiber a)
affDelay t a = forkAff $ delay (Milliseconds t) *> a

delayerSnapper :: forall m.
  MonadAff m =>
  AVar Unit -> m (Snapper m DelayerAction (Maybe String))
delayerSnapper = affSnapper reducer init
  where
  init = Just "Click the button to launch a delayed request"
  reducer (Load put) _ = liftAff $ do
    _ <- affDelay 1000.0 $ put (Loaded "Delayed request completed.")
    pure Nothing
  reducer (Loaded s) _ = pure $ Just s

snapper :: forall m. MonadAff m => AVar Unit -> m (Snapper m Action State)
snapper av = (|&) <$> counterSnapper av <*> delayerSnapper av
