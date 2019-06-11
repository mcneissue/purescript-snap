module Main where

import Prelude

import Data.Array (replicate)
import Data.Either (Either(..), either)
import Data.Foldable (and)
import Data.Functor.Variant (SProxy(..))
import Data.Lens (Lens', Optic', lens)
import Data.Lens.Record (prop)
import Data.Maybe (maybe)
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Profunctor.Monoidal (switch, traversed, partsOfMono)
import Data.Traversable (class Traversable)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import React.Basic (JSX)
import React.Basic.DOM (div, text) as RB
import React.Basic.Events (handler_) as RB
import Snap (snap)
import Snap.Component (Component(..), Component', contraHoist, focus, runMComponent)
import Snap.React (reactTarget, refSnapper)
import Snap.React.Component (debug)
import Snap.React.Component as S
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Dumb little app to demonstrate the concept

type Todo = { done :: Boolean, hovered :: Boolean, editing :: Boolean, value :: String }

-- TODO: Factor this into a magic heterogeneous record thing that looks like `magic { focused: "editing", value: "value" }`
todoEditor :: Component' Effect JSX Todo
todoEditor = l S.input
  where
  l = lens (\r -> { focused: r.editing, value: r.value }) (\s v -> s { editing = v.focused, value = v.value })

todoViewer :: Component Effect JSX Todo Todo
todoViewer = runMComponent $ ado
  done   <- wrap $ focus { done: S.checkbox }
  text   <- wrap $ focus { value: S.text }
  toggle <- wrap $ lcmap (_.hovered) S.conditional
  div    <- wrap $
    Component $ \set s children ->
      RB.div { children
             , onClick: RB.handler_ $ set $ s { editing = true }
             }
  in div [done, text, toggle $ RB.text "hovering"]

by :: forall p a. Profunctor p => (a -> Boolean) -> Optic' p a (Either a a)
by f = dimap (\v -> if f v then Left v else Right v) (either identity identity)

-- A todo item
todo :: Component' Effect JSX Todo
todo = by _.editing $ switch todoEditor todoViewer

all :: forall t. Traversable t => Lens' (t Boolean) Boolean
all = lens and (\s b -> if and s == b then s else b <$ s)

type App = Array Todo

-- The application consists of a bunch of components and a text element showing the overall application state
-- Note how a traversed optic can just be applied to a component, since components are profunctorial
app :: Component' Effect JSX App
app = debug $ traversed (S.divved todo) <> (all >>> partsOfMono (traversed <<< prop (SProxy :: SProxy "done"))) S.checkbox

-- Initial application state consists of three components
state :: App
state = replicate 3 s
  where
  s = { done: true
      , hovered: false
      , editing: false
      , value: "Test value pls ignore"
      }

-- Finding the DOM element we're going to render everything onto
element :: Effect Element
element = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById "container"
  maybe (throwException (error "Couldn't find root element")) pure mc

main :: Effect Unit
main = do
  -- Find the DOM element and create an Ref to hold the application state
  e <- element
  ref <- liftEffect $ Ref.new state
  launchAff_ $ do
    av  <- AVar.empty
    -- Create the state manager and target from the resources above
    let snapper = refSnapper ref av
    let target = reactTarget e av
    -- Snap everything together
    snap snapper (contraHoist launchAff_ $ app) target
