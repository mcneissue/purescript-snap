module Snap.React.Component where

import Prelude

import Data.Functor.Variant (SProxy(..))
import Data.Lens.Record (prop)
import Data.Maybe (maybe)
import Effect (Effect)
import Prim.Row (class Union)
import React.Basic (JSX) as R
import React.Basic.DOM (button, div, img, input, text) as R
import React.Basic.DOM.Events (key, targetChecked, targetValue)
import React.Basic.Events (handler, handler_) as R
import Record as RD
import Record.Builder (build, union)
import Snap.Component ((#!))
import Snap.SYTC.Component (Cmp, Cmp', (<*>!))
import Snap.SYTC.Component as C

-- Some convenience things
unionWith :: forall p q r. Union p q r => Record q -> Record p -> Record r
unionWith = (build <<< union)

setProps :: forall p q r x. Union p q r => (Record r -> x) -> Record q -> (Record p -> x)
setProps f v = f <<< unionWith v

infixl 7 setProps as |=

setChildren :: forall x. ({ children :: Array R.JSX } -> x) -> Array R.JSX -> x
setChildren f cs = f { children: cs }

infixr 6 setChildren as |<

setChild :: forall x. ({ children :: Array R.JSX } -> x) -> R.JSX -> x
setChild f c = setChildren f [c]

infixr 6 setChild as |-

hoverability set _ j = j |= { onMouseOver, onMouseLeave }
  where
  onMouseOver = R.handler_ $ set true
  onMouseLeave = R.handler_ $ set false

clickability set _ j = j |= { onClick }
  where
  onClick = R.handler_ $ set unit

button = clickability <*>! C.pure R.button

counter :: Cmp' Effect R.JSX Int
counter = C.ado
  inc <- button # C.handle_ (_ + 1)
  dec <- button # C.handle_ (_ - 1)
  txt <- text   # C.lcmap show
  in R.div
     |< [ inc |- R.text "+"
        , txt
        , dec |- R.text "-"
        ]

focusability set s j = j |= { onFocus, onBlur }
  where
  onFocus  = R.handler_ $ set true
  onBlur   = R.handler_ $ set false

changeability p e set s j = j |= RD.insert p s { onChange }
  where
  onChange = R.handler e $ maybe (pure unit) set

type InputState
  = { focused :: Boolean, value :: String }

input = C.ado
  focus  <- focusability                     #! prop _focused
  change <- changeability _value targetValue #! prop _value
  in R.input # change # focus
  where
  _focused = SProxy :: _ "focused"
  _value   = SProxy :: _ "value"

checkbox = C.ado
  change <- changeability _checked targetChecked
  in R.input |= { type: "checkbox" } # change
  where
  _checked = SProxy :: _ "checked"

keypressability set _ j = j |= { onKeyPress }
  where
  onKeyPress = R.handler key $ maybe (pure unit) set

text :: forall m u. Cmp m R.JSX String u
text _ = R.text

img _ src = R.img |= { src }

conditional :: forall m u. Cmp m (R.JSX -> R.JSX) Boolean u
conditional _ = if _ then identity else const mempty

debug :: forall m s u. Show s => Cmp m R.JSX s u
debug = C.lcmap show text
