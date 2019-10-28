module Snap.React.Component where

import Prelude

import Data.Functor.Variant (SProxy(..))
import Data.Lens.Record (prop)
import Data.Maybe (maybe)
import Data.Profunctor as P
import Data.Profunctor.Optics (Edit(..))
import Data.Record.Append as RO
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1)
import Prim.Row (class Union)
import Prim.RowList (Cons, Nil)
import React.Basic (JSX) as R
import React.Basic.DOM (CSS)
import React.Basic.DOM (button, div, img, input, text) as R
import React.Basic.DOM.Events (key, targetChecked, targetValue)
import React.Basic.Events (SyntheticEvent)
import React.Basic.Events (handler, handler_) as R
import Record as RD
import Snap.Component ((#!))
import Snap.SYTC.Component (Cmp, Cmp')
import Snap.SYTC.Component as C
import Type.Prelude (class ListToRow, class RowToList)

-- Some convenience things
unionWith :: forall p q r. Union p q r => Record q -> Record p -> Record r
unionWith = flip RD.union

infixl 7 unionWith as :+:

infixl 7 compose as |~

setProps :: forall a b c x. Union a b c => (Record c -> x) -> Record b -> Record a -> x
setProps f v = f <<< unionWith v

infixl 7 setProps as |=

setChildren :: forall x. ({ children :: Array R.JSX } -> x) -> Array R.JSX -> x
setChildren f cs = f { children: cs }

infixr 6 setChildren as |<

setChild :: forall x. ({ children :: Array R.JSX } -> x) -> R.JSX -> x
setChild f c = setChildren f [c]

infixr 6 setChild as |-

-- Graft a boolean state to whether a particular element is hovered
hoverability set _ = flip RO.append { onMouseOver, onMouseLeave }
  where
  onMouseOver  = R.handler_ $ set true
  onMouseLeave = R.handler_ $ set false

-- Emit unit values in response to click events on an element
clickability set _ = flip RO.append { onClick }
  where
  onClick = R.handler_ $ set unit

-- Graft a boolean state to whether a particular element is focused
-- TODO: Make this properly settable (right now we just ignore s)
focusability set _ = flip RO.append { onFocus, onBlur }
  where
  onFocus  = R.handler_ $ set true
  onBlur   = R.handler_ $ set false

-- Graft a state to an element by rendering it through some given
-- attribute, and emitting new ones through onChange
-- fires
changeability a e set s =  RO.append { onChange } <<< RO.append (RD.insert a s { })
  where
  onChange = R.handler e $ maybe (pure unit) set

editability = changeability _value targetValue
  where
  _value = SProxy :: SProxy "value"

checkability = changeability _checked targetChecked
  where
  _checked = SProxy :: SProxy "checked"

-- TODO: Deal with mashing together multiple handlers for the same event using monoidness
--       , some kind of monoided out unionWith thingus is needed

-- Emit characters in response to keypress events on an element
keypressability set _ = flip RO.append { onKeyUp }
  where
  onKeyUp = R.handler key $ maybe (pure unit) set

-- Some common keypresses you might want to look for
enter = keypressability # C.when ((==) "Enter")
escape = keypressability # C.when ((==) "Escape")
tab = keypressability # C.when ((==) "Tab")

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

-- A button that accepts no state and emits unit values
button = C.ado
  c <- clickability
  in R.button |~ c

-- A text node that displays a string and never emits
text :: forall m u. Cmp m R.JSX String u
text _ = R.text

-- A counter that manages a number
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

-- An input that manages whether it is focused and a string value
input = C.ado
  focus  <- focusability #! prop _focused
  change <- editability  #! prop _value
  in R.input |~ change |~ focus
  where
  _focused = SProxy :: SProxy "focused"
  _value   = SProxy :: SProxy "value"

-- A checkbox that manages a boolean
checkbox = C.ado
  change <- checkability
  in R.input |= { type: "checkbox" } |~ change
  where
  _checked = SProxy :: SProxy "checked"

-- An img tag that accepts a URL and never emits
img _ src = R.img |= { src }

-- A component that accepts a boolean and renders a provided
-- element if it is true
conditional :: forall m u. Cmp m (R.JSX -> R.JSX) Boolean u
conditional _ = if _ then identity else const mempty

-- Wrapper around text that can be attached to show-able things
debug :: forall m s u. Show s => Cmp m R.JSX s u
debug = C.lcmap show text
