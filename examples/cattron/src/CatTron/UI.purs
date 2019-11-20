module Examples.CatTron.UI where

import Prelude

import Data.Either (either)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Examples.CatTron.State (State(..), randomGifUrl)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import Snap.React.Component ((|-), (|<), (|=))
import Snap.SYTC.Component (Cmp', Cmp)
import Snap.SYTC.Component as C

reload :: forall s. Cmp Effect JSX s State
reload set _ = R.button |= { onClick } |- R.text "MOAR"
  where
  onClick = handler_ $ do
    set Loading
    launchAff_ $ do
      delay $ Milliseconds $ 1000.0
      result <- randomGifUrl
      liftEffect $ set $ either Error Gif result

view :: forall u. Cmp Effect JSX State u
view _ Start     = R.p |- R.text "Press the button to start loading teh cats"
view _ Loading   = R.p |- R.text "Loading..."
view _ (Error s) = R.p |- R.text ("Error: " <> s)
view _ (Gif src) = R.img { src }

app :: Cmp' Effect JSX State
app = C.ado
  r <- reload
  v <- view
  in R.div |<
     [ R.h1 |<
       [ R.text "CatTron 9000 Cat Gif Viewing Device" ]
     , R.div |- v
     , R.div |- r
     ]
