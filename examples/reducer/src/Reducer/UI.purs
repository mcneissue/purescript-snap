module Examples.Reducer.UI where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Examples.Reducer.State (CounterAction(..), DelayerAction(..), State)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.Component.SYTC (Cmp)
import Snap.Component.SYTC as C
import Snap.React.Component ((|<), (|-))
import Snap.React.Component as RC

app :: Cmp Aff JSX State (Either CounterAction DelayerAction)
app = C.ado
  cntr <- fromEffCmp $ counter # C.dimap fst Left
  dlyr <- delayer # C.dimap snd Right
  in R.div |< [ cntr, dlyr ]

counter :: Cmp Effect JSX Int CounterAction
counter = C.ado
  inc <- RC.button # C.rmap (const Increment)
  dec <- RC.button # C.rmap (const Decrement)
  txt <- RC.text # C.lcmap show
  in R.div
    |< [ inc |- R.text "+"
       , txt
       , dec |- R.text "-"
       ]

delayer :: Cmp Aff JSX (Maybe String) DelayerAction
delayer put = go put
  where
    go = C.ado
      load <- fromEffCmp $ RC.button # C.rmap (const $ Load put)
      txt  <- RC.text # C.lcmap (fromMaybe "Loading...")
      in R.div
        |< [ load |- R.text "Click Me"
           , txt
           ]

fromEffCmp :: forall v s u. Cmp Effect v s u -> Cmp Aff v s u
fromEffCmp = C.contraHoist launchAff_
