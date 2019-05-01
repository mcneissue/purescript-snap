module Main where

import Prelude

import Component (Component(..), contraHoistVoid, refocus)
import Data.Lens (lens)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (SProxy(..), Variant, case_, inj, on)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Basic (Component, JSX, createComponent, make) as R
import React.Basic.DOM (button, div, render, text, input) as R
import React.Basic.DOM.Events as RE
import Snap (Target(..), Snapper, snap)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

data Counter = Increment | Decrement

counter :: Component Effect R.JSX Int Counter
counter = Component \update s -> R.make component { render: render update, initialState: Nothing } { count: s }
  where
    render update self = R.div
      { children: 
        [ R.button { children: [ R.text "Increment" ], onClick: RE.capture_ $ update Increment }
        , R.text $ show self.props.count
        , R.button { children: [ R.text "Decrement" ], onClick: RE.capture_ $ update Decrement }
        ]
      }
    component :: R.Component { count :: Int }
    component = R.createComponent "Counter"

counterReducer :: Int -> Counter -> Int
counterReducer s Increment = s + 1
counterReducer s Decrement = s - 1

data Echoer = Echo String

echoer :: Component Effect R.JSX String Echoer
echoer = Component \update s -> R.make component { render: render update, initialState: Nothing } { input: s }
  where
    render update self = R.div
      { children:
        [ R.input { onChange: RE.capture RE.targetValue $ maybe (pure unit) (update <<< Echo) }
        , R.text $ "You said: " <> self.props.input
        ]
      }

    component :: R.Component { input :: String }
    component = R.createComponent "Echoer"

echoerReducer :: String -> Echoer -> String
echoerReducer _ (Echo s) = s

type EchoCounter = Variant (echoer :: Echoer, counter :: Counter)

echoCounter :: Component Effect R.JSX { echoer :: String, counter :: Int } EchoCounter
echoCounter = refocus clens counter <> refocus elens echoer
  where
    clens = lens _.counter (\_ -> inj (SProxy :: SProxy "counter"))
    elens = lens _.echoer (\_ -> inj (SProxy :: SProxy "echoer"))

ecReducer :: { echoer :: String, counter :: Int } -> EchoCounter -> { echoer :: String, counter :: Int }
ecReducer s =  case_
  # on (SProxy :: SProxy "echoer")  (\a -> s { echoer = echoerReducer s.echoer a })
  # on (SProxy :: SProxy "counter") (\a -> s { counter = counterReducer s.counter a }) 

reactSnap :: forall s u m. MonadAff m => Component Effect R.JSX s u -> (s -> u -> Effect s) -> s -> Element -> m Unit
reactSnap component reducer initialState elm = do
  av  <- liftAff $ AVar.empty
  ref <- liftEffect $ Ref.new initialState
  let snapper = refSnapper (\s u -> liftEffect $ reducer s u) ref av
  liftAff $ snap snapper (contraHoistVoid launchAff_ component) (reactTarget elm av)

refSnapper :: forall s u m. MonadAff m => (s -> u -> m s) -> Ref s -> AVar Unit -> Snapper m s u
refSnapper reducer ref sync = { get, put }
  where
    get = liftEffect $ Ref.read ref
    put u = do
      s  <- liftEffect $ Ref.read ref
      s' <- reducer s u
      liftEffect $ Ref.write s' ref
      _ <- liftAff $ AVar.put unit sync
      pure unit

reactTarget :: forall m. MonadAff m => Element -> AVar Unit -> Target m R.JSX
reactTarget e sync = Target go
  where
    go v = do
      liftEffect $ R.render v e
      _ <- liftAff $ AVar.take sync
      pure (Target go)

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Just e  -> launchAff_ $ reactSnap echoCounter (\s -> pure <<< ecReducer s) { counter: 0, echoer: "" } e
    Nothing -> error "no"
