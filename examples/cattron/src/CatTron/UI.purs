module Examples.CatTron.UI where

import Prelude

import Data.Either (either)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff)
import Effect.Class (liftEffect)
import Examples.CatTron.State (State(..), randomGifUrl)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import Snap.React.Component ((|-), (|<), (|=))
import Snap.SYTC.Component (Cmp', Cmp)
import Snap.SYTC.Component as C

loadGif :: Aff State
loadGif = do
  delay $ Milliseconds $ 1000.0
  result <- randomGifUrl
  pure $ either Error Gif result

reload :: forall s. Cmp Effect JSX s State
reload set _ = R.button |= { onClick } |- R.text "MOAR"
  where
  onClick = handler_ $ set Loading

view :: forall u. Cmp Effect JSX State u
view _ Loading   = R.p |- R.text "Loading..."
view _ (Error s) = R.p |- R.text ("Error: " <> s)
view _ (Gif src) = R.img { src }

component :: Cmp' Effect JSX State
component = C.ado
  r <- reload
  v <- view
  in R.div |<
     [ R.h1 |<
       [ R.text "CatTron 9000 Cat Gif Viewing Device" ]
     , R.div |- r
     , R.div |- v
     ]

app :: Cmp' Effect (Aff JSX) State
app set s = do
  when (s == Loading) load
  pure $ component set s
  where
  load = void $ forkAff $ loadGif >>= set >>> liftEffect