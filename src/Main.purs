module Main where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref (Ref, new)
import React.Basic (Component, JSX, createComponent, make) as R
import React.Basic.DOM (button, div, render, text) as R
import React.Basic.DOM.Events as RE
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Component (Component(..), contraHoist, runComponent)
import Snap (Target(..), snap, modifyRef)

data Action = Increment | Decrement

counter :: forall r r'
         . { ref :: Ref { count :: Int | r} | r' } 
        -> Component (ReaderT { ref :: Ref { count :: Int | r } | r' } Effect) R.JSX Int Action
counter ref = adaptReactComponent ref handler component
  where
    component = Component \update s -> R.make cinfo { render : render update, initialState: Nothing } { count: s }

    render update self = R.div
      { children: 
        [ R.button { children: [ R.text "Increment" ], onClick: RE.capture_ $ update Increment }
        , R.text $ show self.props.count
        , R.button { children: [ R.text "Decrement" ], onClick: RE.capture_ $ update Decrement }
        ]
      }

    cinfo :: R.Component { count :: Int }
    cinfo = R.createComponent "Counter"

    handler Increment = modifyRef (prop (SProxy :: SProxy "count")) (\s -> s + 1)
    handler Decrement = modifyRef (prop (SProxy :: SProxy "count")) (\s -> s - 1)

adaptReactComponent :: forall s s' r a
                     . { ref :: Ref s' | r }
                    -> (a -> ReaderT { ref :: Ref s' | r } Effect Unit)
                    -> Component Effect R.JSX s a 
                    -> Component (ReaderT {ref :: Ref s' | r } Effect) R.JSX s a
adaptReactComponent ref handler component = Component adapted
  where
    adapted update s = runComponent hoisted (\u -> handler u *> update u) s
    hoisted = contraHoist (flip runReaderT ref) component

reactTarget :: forall m. MonadEffect m => Element -> Target m R.JSX
reactTarget e = Target $ Just go
  where
    go v = do
      liftEffect $ R.render v e
      pure $ Target $ Just go

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  ref <- new { count: 0 }
  let s = { ref } 
  case container of
    Just e  -> runReaderT (snap (prop (SProxy :: SProxy "count")) (counter s) (reactTarget e)) s
    Nothing -> error "no"
