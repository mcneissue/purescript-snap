module Main where

import Prelude

import Component (Component(..), contraHoist, runComponent, refocus)
import Data.Lens (lens)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (SProxy(..), Variant, case_, inj, on)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as AVar
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

adaptComponent :: forall n m s u v. Bind m => (s -> u -> m s) -> m s -> (s -> m Unit) -> (m ~> n) -> Component n v s u -> Component m v s u
adaptComponent reducer get set nat component = Component adapted
  where
    adapted :: (u -> m Unit) -> s -> v
    adapted update state = runComponent hoisted (\u -> (get >>= \s -> reducer s u >>= set) *> update u) state
      where
        hoisted = contraHoist nat component

reactSnap :: forall s u. Component Effect R.JSX s u -> (s -> u -> Effect s) -> s -> Element -> Effect Unit
reactSnap component reducer initialState elm = do
  av  <- AVar.empty
  ref <- Ref.new initialState
  let snapper    = reactSnapper av ref
      component' = adaptComponent reducer (Ref.read ref) (flip Ref.write ref) identity component
  snap snapper component' (reactTarget elm av)

reactSnapper :: forall s u. AVar Unit -> Ref s -> Snapper Effect s u
reactSnapper av ref = 
  { get: Ref.read ref
  , put: const $ void $ AVar.put unit av $ const $ pure unit
  }

reactTarget :: Element -> AVar Unit -> Target Effect R.JSX
reactTarget e av = Target go
  where
    go continue v = do
      R.render v e
      _ <- AVar.take av (const continue)
      pure unit

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Just e  -> reactSnap echoCounter (\s -> pure <<< ecReducer s) { counter: 0, echoer: "" } e
    Nothing -> error "no"
