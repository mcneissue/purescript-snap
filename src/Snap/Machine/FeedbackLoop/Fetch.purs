module Snap.Machine.FeedbackLoop.Fetch where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Cont (ContT(..))
import Control.Parallel.Class (parallel, sequential)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Snap.Machine.FeedbackLoop (FeedbackLoop, emptyCont)

data State r e = Loading | Success r | Failure e
data Transition r e = Reload | Succeed r | Fail e

cancelable :: ∀ a. AVar Unit -> Aff a -> Aff (Maybe a)
cancelable av task = sequential $ parallel (Nothing <$ AVar.read av) <|> (Just <$> parallel task)

makeMachine :: ∀ r e.
  AVar Unit ->
  Aff (Either e r) ->
  FeedbackLoop Unit Aff (State r e) (Transition r e)
makeMachine avar load state = update /\ transition
  where
  update = case state of
    Loading -> ContT \cb -> do
      -- Cancel any previous requests
      _ <- AVar.tryTake avar
      AVar.put unit avar
      -- Start new request
      _ <- AVar.take avar
      value <- cancelable avar load
      case value of
        Nothing -> pure unit
        Just v -> either (cb <<< Fail) (cb <<< Succeed) v
    Success r -> emptyCont
    Failure e -> emptyCont
  transition t = case t of
    Reload -> Loading
    Succeed r -> Success r
    Fail e -> Failure e

machine :: ∀ e r. Aff (Either e r) -> Aff (FeedbackLoop Unit Aff (State r e) (Transition r e))
machine load = do
  avar <- AVar.empty
  pure $ makeMachine avar load

initialState :: ∀ r e. State r e
initialState = Loading
