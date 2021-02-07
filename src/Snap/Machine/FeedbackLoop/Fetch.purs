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
import Snap.Machine.FeedbackLoop (FeedbackLoop)

data State r e = Loading | Success r | Failure e
data Transition r e = Reload | Succeed r | Fail e

cancelable :: ∀ a. AVar Unit -> Aff a -> Aff (Maybe a)
cancelable av task = sequential $ parallel (Nothing <$ AVar.read av) <|> (Just <$> parallel task)

machine :: ∀ res err.
  AVar Unit ->
  Aff (Either err res) ->
  FeedbackLoop Unit Aff (State res err) (Transition res err)
machine avar load state = update /\ transition
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

initialState :: ∀ r e. State r e
initialState = Loading

emptyCont :: ∀ x f a. Applicative f => Monoid x => ContT x f a
emptyCont = ContT \_ -> pure mempty
