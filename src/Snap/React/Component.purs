module Snap.React.Component where

import Prelude

import Data.Functor.Variant (SProxy(..))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor as P
import Data.Profunctor.Optics (Edit(..), Transactional, atomically)
import Data.Record.Append (class AppendRecord)
import Data.Record.Append as RO
import Data.Record.Extras as RE
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1)
import Prim.Row (class Cons, class Lacks, class Union)
import React.Basic (JSX) as R
import React.Basic.DOM (button, div, img, input, text) as R
import React.Basic.DOM (Props_button, Props_input, SharedProps, Props_img)
import React.Basic.DOM.Events (key, targetChecked, targetValue) as R
import React.Basic.Events (EventFn, SyntheticEvent)
import React.Basic.Events (handler, handler_) as R
import Record as RD
import Snap.Component ((#!), ($!))
import Snap.SYTC.Component (Cmp, Cmp')
import Snap.SYTC.Component as C

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

-- Extensible components
type Affordance r s u = forall ri ro. AppendRecord ri r ro => Cmp Effect ({ | ri } -> { | ro }) s u
type Affordance' r s = Affordance r s s

-- Graft a boolean state to whether a particular element is hovered
hovering :: forall s.
  Affordance
    ( onMouseLeave :: EffectFn1 SyntheticEvent Unit
    , onMouseOver  :: EffectFn1 SyntheticEvent Unit )
    s Boolean
hovering set _ = flip RO.append { onMouseOver, onMouseLeave }
  where
  onMouseOver  = R.handler_ $ set true
  onMouseLeave = R.handler_ $ set false

-- Emit unit values in response to click events on an element
clicked :: forall s.
  Affordance
    ( onClick :: EffectFn1 SyntheticEvent Unit )
    s Unit
clicked set _ = flip RO.append { onClick }
  where
  onClick = R.handler_ $ set unit

-- Graft a boolean state to whether a particular element is focused
-- TODO: Make this properly settable (right now we just ignore s)
focused :: forall s.
  Affordance
    ( onBlur :: EffectFn1 SyntheticEvent Unit
    , onFocus :: EffectFn1 SyntheticEvent Unit )
    s Boolean
focused set _ = flip RO.append { onFocus, onBlur }
  where
  onFocus  = R.handler_ $ set true
  onBlur   = R.handler_ $ set false

-- An affordance for things that have some state that can be modified
-- through some event
altered :: forall attr attr' k e.
  -- The part of the attribute dictionary that models the input element's value
  IsSymbol k =>
  Lacks k () =>
  Cons k e () attr =>

  -- The rest of the attribute dictionary
  AppendRecord
    attr
    ( onChange :: EffectFn1 SyntheticEvent Unit )
    attr' =>

  SProxy k ->
  EventFn SyntheticEvent (Maybe e) ->
  Affordance' attr' e
altered k e set s ri =  RO.append ri (RO.append (RE.singleton k s) { onChange })
  where
  onChange = R.handler e $ maybe (pure unit) set

edited ::
  Affordance
    ( onChange :: EffectFn1 SyntheticEvent Unit
    , value :: String )
    String String
edited = altered _value R.targetValue
  where
  _value = SProxy :: SProxy "value"

checked ::
  Affordance
    ( onChange :: EffectFn1 SyntheticEvent Unit
    , checked :: Boolean )
    Boolean Boolean
checked = altered _checked R.targetChecked
  where
  _checked = SProxy :: SProxy "checked"

-- Emit characters in response to keypress events on an element
keypressed :: forall s.
  Affordance
    ( onKeyUp :: EffectFn1 SyntheticEvent Unit )
    s String
keypressed set _ = flip RO.append { onKeyUp }
  where
  onKeyUp = R.handler R.key $ maybe (pure unit) set

type Keypress = forall s.
  Affordance
    ( onKeyUp :: EffectFn1 SyntheticEvent Unit )
    s String

-- Some common keypresses you might want to look for
enterPressed :: Keypress
enterPressed = keypressed # C.when ((==) "Enter")

escapePressed :: Keypress
escapePressed = keypressed # C.when ((==) "Escape")

tabPressed :: Keypress
tabPressed = keypressed # C.when ((==) "Tab")

-- Given
--
-- 1. change: an affordance that emits values
-- 2. revert: an affordance that emits anything
-- 3. save: an affordance that emits anything
--
-- produces an affordance that emits transactional edits of the given value
transacted :: forall a b c d s m.
  { revert :: Cmp m (a -> b) s _
  , save   :: Cmp m (b -> c) s _
  , change :: Cmp' m (c -> d) s | _ } ->
  Cmp' m (a -> d) (Transactional s)
transacted { change, save, revert } = atomically $! C.ado
  c <- change #! P.rmap Change
  s <- save   #! P.rmap (const Save)
  r <- revert #! P.rmap (const Revert)
  in c |~ s |~ r

-- A button that accepts no state and emits unit values
button :: forall s ri ro x.
  AppendRecord ri ( onClick :: EffectFn1 SyntheticEvent Unit ) ro =>
  Union ro x (SharedProps Props_button) =>
  Cmp Effect ({ | ri } -> R.JSX) s Unit
button = C.ado
  c <- clicked
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

-- An input that manages a boolean value representing whether it is focused,
-- and a string value representing its contents
input :: forall a b c.
  AppendRecord a
    ( onBlur :: EffectFn1 SyntheticEvent Unit
    , onFocus :: EffectFn1 SyntheticEvent Unit )
    b =>
  AppendRecord b
    ( onChange :: EffectFn1 SyntheticEvent Unit
    , value :: String )
    c =>
  Union c _ (SharedProps Props_input) =>
  Cmp' Effect ({ | a } -> R.JSX) { focused :: Boolean, value :: String }
input = C.ado
  focus  <- focused #! prop (SProxy :: SProxy "focused")
  change <- edited  #! prop (SProxy :: SProxy "value")
  in R.input |~ change |~ focus

-- A checkbox that manages a boolean
checkbox :: forall a b c.
  AppendRecord a
    ( onChange :: EffectFn1 SyntheticEvent Unit
    , checked :: Boolean )
    b =>
  Union b ( type :: String ) c =>
  Union c _ (SharedProps Props_input) =>
  Cmp' Effect ({ | a } -> R.JSX) Boolean
checkbox = C.ado
  change <- checked
  in R.input |= { type: "checkbox" } |~ change
  where
  _checked = SProxy :: SProxy "checked"

-- An img tag that accepts a URL and never emits
img :: forall a b x u.
  Union a ( src :: String) b =>
  Union b x (SharedProps Props_img) =>
  Cmp Effect ({ | a } -> R.JSX) String u
img _ src = R.img |= { src }

-- A component that accepts a boolean and renders a provided
-- element if it is true
conditional :: forall m u. Cmp m (R.JSX -> R.JSX) Boolean u
conditional _ = if _ then identity else const mempty

-- Wrapper around text that can be attached to show-able things
debug :: forall m s u. Show s => Cmp m R.JSX s u
debug = C.lcmap show text
