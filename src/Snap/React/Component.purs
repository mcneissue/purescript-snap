module Snap.React.Component where

import Effect (Effect)
import Effect.Aff.Compat (EffectFn1)
import Prelude (identity, (<<<), not, (+), (-), const, ($), mempty, class Show, show, Unit, unit)
import Prim.Row (class Union)
import React.Basic (JSX) as R
import React.Basic.DOM (Props_button, Props_input, Props_img)
import React.Basic.DOM (button, div, img, input, text) as R
import React.Basic.DOM.Internal (SharedProps)
import React.Basic.Events (SyntheticEvent)
import React.Basic.Events (handler_) as R
import Record.Builder (build, union)
import Snap.SYTC.Component (Cmp, Cmp', apply, handle, map, pure)
import Snap.SYTC.Component (lcmap) as C

-- Some convenience things
unionPropsWith :: forall p q r. Union p q r => Record q -> Record p -> Record r
unionPropsWith = (build <<< union)

setChildren :: forall x. ({ children :: Array R.JSX } -> x) -> Array R.JSX -> x
setChildren f cs = f { children: cs }

infixl 3 setChildren as |<

button ::
  forall s x y z.
  Union y z (SharedProps Props_button) =>
  Union x ( onClick :: EffectFn1 SyntheticEvent Unit ) y =>
  Cmp Effect (Record x -> R.JSX) s Unit
button set _ =
  R.button
    <<< unionPropsWith { onClick }
  where
  onClick = R.handler_ $ set unit

counter :: Cmp' Effect R.JSX Int
counter = ado
  inc <- handle (const (_ + 1)) button
  dec <- handle (const (_ - 1)) button
  txt <- C.lcmap show text
  in
  R.div |<
    [ inc |< [ R.text "+" ]
    , txt
    , dec |< [ R.text "-" ]
    ]

type InputState
  = { focused :: Boolean, value :: String }

foreign import focusedInputComponent :: (InputState -> Effect Unit) -> InputState -> R.JSX

input :: Cmp' Effect R.JSX InputState
input = focusedInputComponent

checkbox ::
  forall x y z.
  Union y z (SharedProps Props_input) =>
  Union x ( checked :: Boolean, onChange :: EffectFn1 SyntheticEvent Unit, type :: String ) y =>
  Cmp' Effect ({ | x } -> R.JSX) Boolean
checkbox set s =
  R.input
    <<< unionPropsWith
        { type: "checkbox"
        , checked: s
        , onChange: R.handler_ $ set (not s)
        }

text :: forall m u. Cmp m R.JSX String u
text _ = R.text

img ::
  forall u x y z.
  Union y z (SharedProps Props_img) =>
  Union x ( src :: String ) y =>
  Cmp Effect ({ | x } -> R.JSX) String u
img _ s = R.img <<< unionPropsWith { src: s }

conditional :: forall m u. Cmp m (R.JSX -> R.JSX) Boolean u
conditional _ = bool identity (const mempty)
  where
  bool x y b = if b then x else y

debug :: forall m s u. Show s => Cmp m R.JSX s u
debug = C.lcmap show text
