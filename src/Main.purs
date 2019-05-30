module Main where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import React.Basic (JSX) as R
import Snap (snap)
import Snap.Component (Component', contraHoistVoid)
import Snap.React (reactTarget, refSnapper)
import Snap.React.Component (input, twoCountersAndAnInput)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

reactSnap :: forall s m
           . MonadAff m
          => (forall a. m a -> Effect Unit)
          -> Component' Effect R.JSX s
          -> s
          -> Element
          -> m Unit
reactSnap nat component s0 elm = do
  av  <- liftAff $ AVar.empty
  ref <- liftEffect $ Ref.new s0
  let snapper = refSnapper ref av
  snap snapper (contraHoistVoid nat component) (reactTarget elm av)

runApp :: forall r a. { delayTime :: Number | r } -> ReaderT { delayTime :: Number | r } Aff a -> Effect Unit
runApp r = launchAff_ <<< flip runReaderT r

element :: Effect Element
element = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById "container"
  maybe (throwException (error "Couldn't find root element")) pure mc

main :: Effect Unit
main = do
  let s0 = { counter1: 0, counter2: 42, input: "Hello" }
  e <- element
  ref <- liftEffect $ Ref.new s0
  launchAff_ $ do
    av  <- AVar.empty
    let snapper = refSnapper ref av
    let target = reactTarget e av
    snap snapper (contraHoistVoid launchAff_ twoCountersAndAnInput) target
