module Examples.Reducer.UI where

import Prelude

import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Examples.Reducer.State (CounterAction(..), DelayerAction(..), State, counterReducer, delayerReducer)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.React.Component ((|<), (|-))
import Snap.React.Component as RC
import Snap.SYTC.Component (Cmp, Cmp')
import Snap.SYTC.Component as C
import Snap.Component ((#!))

app :: Cmp' Aff JSX State
app = C.ado
  cntr <- fromEffCmp $ C.handle counterReducer counter #! prop (SProxy :: SProxy "counter")
  dlyr <- C.handleM delayerReducer delayer #! prop (SProxy :: SProxy "delayer")
  in R.div |< [ cntr, dlyr ]

counter :: Cmp Effect JSX Int CounterAction
counter = C.ado
  inc <- RC.button' Increment
  dec <- RC.button' Decrement
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
      load <- fromEffCmp $ RC.button' $ Load put
      txt  <- RC.text # C.lcmap (fromMaybe "Loading...")
      in R.div
        |< [ load |- R.text "Click Me"
           , txt
           ]

fromEffCmp :: forall v s u. Cmp Effect v s u -> Cmp Aff v s u
fromEffCmp = C.contraHoist launchAff_