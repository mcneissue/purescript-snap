module Examples.Reducer.UI where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Examples.Reducer.State (Action, CounterAction(..), DState(..), DUpdate(..), State)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.Component.SYTC (Cmp)
import Snap.Component.SYTC as C
import Snap.React.Component (debug, (|-), (|<))
import Snap.React.Component as RC
import Snap.Mealy as Mealy

component :: Cmp Effect JSX State Action
component = C.ado
  cntr <- counter # C.dimap fst Left
  dlyr <- delayer # C.dimap snd Right
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
      load <- RC.button # C.rmap (const Mealy.Load)
      txt  <- RC.text # C.lcmap mkLabel
      dbg <- debug
      in R.div
        |< [ load |- R.text "Click Me Pls"
           , txt
           , dbg
           ]
    mkLabel s = case s of
      Mealy.Loading -> "Loading"
      Mealy.Success r -> r
      Mealy.Idle -> "Click the button!"
      Mealy.Failure e -> absurd e

fromEffCmp :: forall v s u. Cmp Effect v s u -> Cmp Aff v s u
fromEffCmp = C.contraHoist launchAff_
