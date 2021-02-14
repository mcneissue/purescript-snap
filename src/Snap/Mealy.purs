module Snap.Mealy where

import Prelude

import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Tuple.Nested (type (/\), (/\))

import Control.Monad.Cont (ContT(..), runContT)
import Effect (Effect)
import Effect.Class.Console (error)
import Snap.Component.SYTC (Cmp)
import Snap.Machine.FeedbackLoop (emptyCont)

-- {{{ Continutation monad stuff

type ECont = ContT Unit Effect

-- }}}

-- {{{ Machine step

data Transition s e = Yes s e | No

type Step s i v e = v /\ (i -> Transition s e)

-- mapStep :: ∀ i v e a b.
--   (a -> b) -> Step s i v e -> Step s i v e
-- mapStep = ?1

-- A coalgebra (wow, such math)
type Machine s i v e = s -> Step s i v e

-- Greatest fixpoint of coalgebra
newtype Behavior i v e = Behavior (Step (Behavior i v e) i v e)

-- Effectful machines
type EMachine s i = Machine s i (ECont i) (ECont i)

type EBehavior i = Behavior i (ECont i) (ECont i)

unfoldMachine :: ∀ s i v e.
  Machine s i v e -> s -> Behavior i v e
unfoldMachine machine = go
  where
  go s = Behavior $ case machine s of
    v /\ f -> v /\ \i -> f i # case _ of
      No -> No
      Yes s' e -> Yes (go s') e

encapsulate :: ∀ s u v.
  EMachine s u -> Cmp Effect v s u -> s -> ECont v
encapsulate m render s = ContT $ \cb -> do
  let
    handleNewState current = do
      -- Get the background task for the current state
      let background /\ _ = m current
      -- Run the background task
      runContT background handleUpdate
      -- Render the new UI
      cb $ render handleUpdate current

    handleUpdate u = do
      -- Run the machine to get the transition
      let _ /\ transition = m s
      -- Run the transition to get the next state and the side effect
      case transition u of
        No -> error "Bad transition was applied to the machine, to you sir we say: 🖕"
        Yes nextState effect -> do
          -- Render the new UI
          handleNewState nextState
          -- Run the side effect
          runContT effect handleUpdate

  handleNewState s

-- }}}


-- {{{ Fetch machine

type DogImageUrl = String
type Error = String

data DogFetchState = Idle | Loading | Success DogImageUrl | Failure Error
data DogFetchUpdate = Load | Cancel | Succeed DogImageUrl | Fail Error

dogFetch ::
  ECont (Error \/ DogImageUrl) ->
  EMachine DogFetchState DogFetchUpdate
dogFetch fetch = case _ of

  Idle -> pure Load /\ case _ of
    Load -> Yes Loading $ fetch <#> either Fail Succeed
    _ -> No

  Loading -> emptyCont /\ case _ of
    Succeed url -> Yes (Success url) emptyCont
    Fail error -> Yes (Failure error) emptyCont
    _ -> No

  Success url -> emptyCont /\ case _ of
    Load -> Yes Idle emptyCont
    _ -> No

  Failure error -> emptyCont /\ case _ of
    Load -> Yes Idle emptyCont
    _ -> No

-- }}}
