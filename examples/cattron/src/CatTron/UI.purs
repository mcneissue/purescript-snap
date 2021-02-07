module Examples.CatTron.UI where

import Prelude

import Effect (Effect)
import Examples.CatTron.State (AppTransition, AppState)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import Snap.Component.SYTC (Cmp)
import Snap.Component.SYTC as C
import Snap.Machine.FeedbackLoop (State(..), Transition(..))
import Snap.React.Component ((|-), (|<), (|=))

reload :: forall s. Cmp Effect JSX s AppTransition
reload set _ = R.button |= { onClick } |- R.text "MOAR"
  where
  onClick = handler targetValue $ \_ -> set Reload

view :: forall u. Cmp Effect JSX AppState u
view _ Loading   = R.p |- R.text "Loading..."
view _ (Failure s) = R.p |- R.text ("Failure: " <> s)
view _ (Success src) = R.img { src }

component :: Cmp Effect JSX AppState AppTransition
component = C.ado
  r <- reload
  v <- view
  in R.div |<
     [ R.h1 |<
       [ R.text "CatTron 9000 Cat Success Viewing Device" ]
     , R.div |- r
     , R.div |- v
     ]
