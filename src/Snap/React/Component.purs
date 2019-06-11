module Snap.React.Component where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Profunctor (lcmap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff)
import Effect.Aff.Class (liftAff)
import React.Basic (Component, JSX, createComponent, make) as RB
import React.Basic.DOM (button, div, div_, img, input, text) as RB
import React.Basic.Events (handler_) as RB
import Snap.Component (Component(..), Component', MComponent(..), runMComponent)

button :: forall s. Component Effect (Array RB.JSX -> RB.JSX) s Unit
button = Component
  \set _ children -> RB.button { children, onClick: RB.handler_ $ set unit }

counter :: Component' Effect RB.JSX Int
counter = Component \set s -> RB.div_
  [ RB.button { children: [ RB.text "Increment" ], onClick: RB.handler_ $ set (s + 1) }
  , RB.text $ show s
  , RB.button { children: [ RB.text "Decrement" ], onClick: RB.handler_ $ set (s - 1) }
  ]

type InputState = { focused :: Boolean, value :: String }
foreign import focusedInputComponent :: (InputState -> Effect Unit) -> InputState -> RB.JSX

input :: Component' Effect RB.JSX InputState
input = Component focusedInputComponent

checkbox :: Component' Effect RB.JSX Boolean
checkbox = Component cmp
  where
  cmp set s = RB.input
    { type: "checkbox"
    , checked: s
    , onChange: RB.handler_ $ set (not s)
    }

text :: forall m u. Component m RB.JSX String u
text = Component \_ s -> RB.text s

image :: forall m u. Component m RB.JSX String u
image = Component \_ s -> RB.img { src: s }

divved :: forall m s u. Component m RB.JSX s u -> Component m RB.JSX s u
divved = MComponent >>> map (\x -> RB.div { children: [x] }) >>> runMComponent

conditional :: forall m u. Component m (RB.JSX -> RB.JSX) Boolean u
conditional = Component \_ s -> if s then identity else const mempty

debug :: forall m s u. Show s => Component m RB.JSX s u -> Component m RB.JSX s u
debug c = (divved c) <> (divved $ lcmap show $ text)

type DelayState = Maybe String

delayer :: Component' Effect RB.JSX DelayState
delayer = Component \set s -> RB.make component { render, initialState: s, didMount: didMount set } { message: s }
  where
  didMount set self = maybe (wait *> set (Just "Ready!")) (const $ pure unit) $ self.props.message

  wait = launchAff $ liftAff $ delay (Milliseconds 5.0)

  render self = RB.div
    { children:
      [ RB.button { children: [ RB.text "Delayed Message" ] }
      , RB.text $ fromMaybe "Loading..." self.props.message
      ]
    }

  component :: RB.Component { message :: DelayState }
  component = RB.createComponent "Delayer"
