module Snap.React.Component where

import Prelude

import Control.Category (compose)
import Data.Functor.Variant (SProxy(..))
import Data.Lens.Record (prop)
import Data.Maybe (maybe)
import Data.Profunctor as P
import Data.Profunctor.Optics (Edit(..))
import Effect (Effect)
import Prim.Row (class Union)
import React.Basic (JSX) as R
import React.Basic.DOM (button, div, img, input, text) as R
import React.Basic.DOM.Events (key, targetChecked, targetValue)
import React.Basic.Events (handler, handler_) as R
import Record as RD
import Snap.Component ((#!))
import Snap.SYTC.Component (Cmp, Cmp', (<*>!))
import Snap.SYTC.Component as C

-- Some convenience things
unionWith :: forall p q r. Union p q r => Record q -> Record p -> Record r
unionWith = flip RD.union

infixl 7 unionWith as :+:

infixl 7 compose as |~

setProps f v = f <<< unionWith v

infixl 7 setProps as |=

setChildren :: forall x. ({ children :: Array R.JSX } -> x) -> Array R.JSX -> x
setChildren f cs = f { children: cs }

infixr 6 setChildren as |<

setChild :: forall x. ({ children :: Array R.JSX } -> x) -> R.JSX -> x
setChild f c = setChildren f [c]

infixr 6 setChild as |-

hoverability set _ = (:+:) { onMouseOver, onMouseLeave }
  where
  onMouseOver = R.handler_ $ set true
  onMouseLeave = R.handler_ $ set false

clickability set _ = (:+:) { onClick }
  where
  onClick = R.handler_ $ set unit

focusability set s = (:+:) { onFocus, onBlur }
  where
  onFocus  = R.handler_ $ set true
  onBlur   = R.handler_ $ set false

changeability p e set s = (_ :+: { onChange } :+: RD.insert p s {})
  where
  onChange = R.handler e $ maybe (pure unit) set

-- Some common targets of changeability
changeables =
  { value  : changeability _value targetValue
  , checked: changeability _checked targetChecked
  }
  where
  _value = SProxy :: _ "value"
  _checked = SProxy :: _ "checked"

-- TODO: Deal with mashing together multiple handlers for the same event using monoidness
--       , some kind of monoided out unionWith thingus is needed

-- Emit characters in response to keypress events on an element
keypressability set _ = (<>) { onKeyUp }
  where
  onKeyUp = R.handler key $ maybe (pure unit) set

-- Some common keypresses you might want to look for
keys =
  { enter : keypressability # C.when ((==) "Enter")
  , escape: keypressability # C.when ((==) "Escape")
  , tab   : keypressability # C.when ((==) "Tab")
  }

-- Given an affordance that emits values, and two
-- other affordances that simply emit anything,
-- interpret emissions of the former as tentative
-- updates, and the emissions of the latter two
-- as saves and reverts
transactionality { change, save, revert } = C.ado
  c <- change #! P.rmap Change
  s <- save   #! P.rmap (const Save)
  r <- revert #! P.rmap (const Revert)
  in c |~ s |~ r

button = C.ado
  c <- clickability
  in R.button |~ c

text :: forall m u. Cmp m R.JSX String u
text _ = R.text

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

type InputState
  = { focused :: Boolean, value :: String }

input = C.ado
  focus  <- focusability      #! prop _focused
  change <- changeables.value #! prop _value
  in R.input |~ change |~ focus
  where
  _focused = SProxy :: _ "focused"
  _value   = SProxy :: _ "value"

checkbox = C.ado
  change <- changeables.checked
  in R.input |= { type: "checkbox" } |~ change
  where
  _checked = SProxy :: _ "checked"

img _ src = R.img |= { src }

conditional :: forall m u. Cmp m (R.JSX -> R.JSX) Boolean u
conditional _ = if _ then identity else const mempty

debug :: forall m s u. Show s => Cmp m R.JSX s u
debug = C.lcmap show text
