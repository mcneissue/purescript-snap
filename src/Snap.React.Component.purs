module Snap.React.Component where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff)
import Effect.Aff.Class (liftAff)
import React.Basic (Component, JSX, createComponent, make) as R
import React.Basic.DOM (button, div, text, input) as R
import React.Basic.DOM.Events as RE
import Snap.Component (Component(..), Component', refocusAll)

counter :: Component' Effect R.JSX Int
counter = Component \update s -> R.div
  { children:
    [ R.button { children: [ R.text "Increment" ], onClick: RE.capture_ $ update (s + 1) }
    , R.text $ show s
    , R.button { children: [ R.text "Decrement" ], onClick: RE.capture_ $ update (s - 1) }
    ]
  }

input :: Component' Effect R.JSX String
input = Component \update s -> R.input
  { value: s
  , onChange: RE.capture RE.targetValue $ maybe (pure unit) update
  }

twoCountersAndAnInput :: Component' Effect R.JSX { counter1 :: Int, counter2 :: Int, input :: String }
twoCountersAndAnInput = let r = refocusAll { counter1, counter2, input } in r.counter1 <> r.counter2 <> r.input
  where
  counter1 = counter
  counter2 = counter

type DelayState = Maybe String

delayer :: Component' Effect R.JSX DelayState
delayer = Component \update s -> R.make component { render, initialState: s, didMount: didMount update } { message: s }
  where
  didMount update self = maybe (wait *> update (Just "Ready!")) (const $ pure unit) $ self.props.message

  wait = launchAff $ liftAff $ delay (Milliseconds 5.0)

  render self = R.div
    { children:
      [ R.button { children: [ R.text "Delayed Message" ] }
      , R.text $ fromMaybe "Loading..." self.props.message
      ]
    }

  component :: R.Component { message :: DelayState }
  component = R.createComponent "Delayer"
