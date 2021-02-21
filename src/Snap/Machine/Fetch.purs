module Snap.Machine.Fetch where

import Prelude

import Control.K as K
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Snap.Machine (EMachine)
import Snap.Machine.Transition (Transition(..))

data FetchState e r = Idle | Loading | Success r | Failure e

derive instance genericFetchState :: Generic (FetchState e r) _

instance showFetchState :: (Show e, Show r) => Show (FetchState e r) where
  show = genericShow

data FetchUpdate e r = Load | Cancel | Succeed r | Fail e

derive instance genericFetchUpdate :: Generic (FetchUpdate e r) _

instance showFetchUpdate :: (Show e, Show r) => Show (FetchUpdate e r) where
  show = genericShow

fetchMachine :: forall e r.
  K.EK (e \/ r) ->
  EMachine (FetchState e r) (FetchUpdate e r)
fetchMachine fetch = case _ of
  Idle -> case _ of
    Load -> loading
    _ -> No
  Loading -> case _ of
    Succeed url -> Yes (Success url) K.empty
    Fail error -> Yes (Failure error) K.empty
    _ -> No
  Success url -> case _ of
    Load -> loading
    _ -> No
  Failure error -> case _ of
    Load -> loading
    _ -> No
  where
  loading = Yes Loading $ (fetch K.<#> either Fail Succeed)
