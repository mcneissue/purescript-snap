module Main where

import Prelude

import Data.Array (replicate)
import Data.Maybe (maybe)
import Data.Profunctor.Monoidal (traversed)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import React.Basic (JSX)
import Snap (snap)
import Snap.Component (Component', contraHoist, focus)
import Snap.React (reactTarget, refSnapper)
import Snap.React.Component (counter, debug, input)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Dumb little app to demonstrate the concept

type ItemState = { counter :: Int, input :: String }
type AppState = Array ItemState

-- An item consists of a counter and an input
item :: Component' Aff JSX ItemState
item =  contraHoist launchAff_
     $  focus { counter } -- The pun here might be confusing, but this is `focus { counter: counter }`, which focuses a counter component on the "counter" property of the state
     <> focus { input }

-- The application consists of a bunch of components and a text element showing the overall application state
-- Note how a traversed optic can just be applied to a component, since components are profunctorial
app :: Component' Aff JSX AppState
app = debug $ traversed item

-- Initial application state consists of three components
state :: AppState
state = replicate 3 { counter: 42, input: "Hello, World" }

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
    snap snapper app target
