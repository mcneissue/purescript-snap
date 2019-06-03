module Snap.React.Component where

import Prelude

import Data.Array (updateAt)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff)
import Effect.Aff.Class (liftAff)
import Partial.Unsafe (unsafePartial)
import React.Basic (Component, JSX, createComponent, make) as R
import React.Basic.DOM (button, div, img, input, text) as R
import React.Basic.DOM.Events as RE
import Snap.Component (Component(..), Component', MComponent(..), runMComponent)

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

button :: forall s. Component Effect R.JSX s Unit
button = Component \update _ -> R.button
  { onClick: RE.capture_ $ update unit
  }

checkbox :: Component' Effect R.JSX Boolean
checkbox = Component \update s -> R.input
  { type: "checkbox"
  , checked: s
  , onChange: RE.capture_ $ update (not s)
  }

text :: forall m u. Component m R.JSX String u
text = Component \_ s -> R.text s

image :: forall m u. Component m R.JSX String u
image = Component \_ s -> R.img { src: s }

divved :: forall m s u. Component m R.JSX s u -> Component m R.JSX s u
divved = MComponent >>> map (\x -> R.div { children: [x] }) >>> runMComponent

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
