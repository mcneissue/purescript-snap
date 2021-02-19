module Examples.Reducer.UI where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Data.Variant (inj)
import Effect (Effect)
import Examples.Reducer.State (Action, CounterAction(..), DState, DUpdate, State)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.Component.SYTC (Cmp)
import Snap.Component.SYTC as C
import Snap.Machine.Fetch as Fetch
import Snap.React.Component (debug, (|-), (|<))
import Snap.React.Component as RC

component :: Cmp Effect JSX State Action
component = C.ado
  cntr <- counter # C.dimap _.counter (inj (SProxy :: _ "counter"))
  dlyr <- delayer # C.dimap _.delayer (inj (SProxy :: _ "delayer"))
  dbg <- debug
  in R.div |< [ cntr, dlyr, dbg ]

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

delayer :: Cmp Effect JSX DState DUpdate
delayer put = go put
  where
    go = C.ado
      load <- RC.button # C.rmap (const Fetch.Load)
      txt  <- RC.text # C.lcmap mkLabel
      dbg <- debug
      in R.div
        |< [ load |- R.text "Click Me Pls"
           , txt
           , dbg
           ]
    mkLabel s = case s of
      Fetch.Loading -> "Loading"
      Fetch.Success r -> r
      Fetch.Idle -> "Click the button!"
      Fetch.Failure e -> absurd e
