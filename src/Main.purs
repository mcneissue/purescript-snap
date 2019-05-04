module Main where

import Prelude

import Component (Component(..), contraHoistVoid, refocus)
import Control.Monad.Reader (class MonadReader, ReaderT, asks, runReaderT)
import Data.Lens (lens)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Variant (SProxy(..), Variant, case_, default, inj, on)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, forkAff)
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

type EchoCounter r = Variant (echoer :: Echoer, counter :: Counter | r)

echoCounter :: forall r a. Component Effect R.JSX { echoer :: String, counter :: Int | r } (EchoCounter a)
echoCounter = refocus clens counter <> refocus elens echoer
  where
    clens = lens _.counter (\_ -> inj (SProxy :: SProxy "counter"))
    elens = lens _.echoer (\_ -> inj (SProxy :: SProxy "echoer"))

ecReducer :: forall r a. { echoer :: String, counter :: Int | r } -> EchoCounter a -> { echoer :: String, counter :: Int | r }
ecReducer s =  default s
  # on (SProxy :: SProxy "echoer")  (\a -> s { echoer = echoerReducer s.echoer a })
  # on (SProxy :: SProxy "counter") (\a -> s { counter = counterReducer s.counter a })

data Delay = Load (Delay -> Effect Unit) | Loaded String
type DelayState r = { delayer :: Maybe String | r }

delayer :: forall r. Component Effect R.JSX (DelayState r) Delay
delayer = Component \update s -> R.make component { render: render update, initialState: Nothing } { message: s }
  where
    render update self = R.div
      { children:
        [ R.button { children: [ R.text "Delayed Message" ], onClick: RE.capture_ $ update (Load update) }
        , R.text $ fromMaybe "Loading..." self.props.message.delayer
        ]
      }
    component :: forall z. R.Component { message :: DelayState z }
    component = R.createComponent "Delayer"

delayReducer :: forall r m. MonadReader { delayTime :: Number | r } m => MonadAff m => Maybe String -> Delay -> m (Maybe String)
delayReducer _ (Load put) = do
  t <- asks _.delayTime
  _ <- liftAff $ forkAff $ delay (Milliseconds t) *> liftEffect (put $ Loaded "Done")
  pure Nothing
delayReducer _ (Loaded m) = pure $ Just m

type RootState = { echoer :: String, counter :: Int, delayer :: Maybe String }
type RootActions = Variant (echoer :: Echoer, counter :: Counter, delayer :: Delay)

rootComponent :: Component Effect R.JSX RootState RootActions
rootComponent = echoCounter <> refocus dlens delayer
  where
    dlens  = lens identity (\_ -> inj (SProxy :: SProxy "delayer"))

rootReducer :: forall r m. MonadReader { delayTime :: Number | r } m => MonadAff m => RootState -> RootActions -> m RootState
rootReducer s = case_
  # on (SProxy :: SProxy "counter") (\a -> pure $ s { counter = counterReducer s.counter a } )
  # on (SProxy :: SProxy "echoer")  (\a -> pure $ s { echoer  = echoerReducer s.echoer a })
  # on (SProxy :: SProxy "delayer") (\a -> (\x -> s { delayer = x}) <$> delayReducer s.delayer a )

reactSnap :: forall s u m
           . MonadAff m 
          => (forall a. m a -> Effect Unit) 
          -> Component Effect R.JSX s u 
          -> (s -> u -> m s) 
          -> s 
          -> Element 
          -> m Unit
reactSnap nat component reducer initialState elm = do
  av  <- liftAff $ AVar.empty
  ref <- liftEffect $ Ref.new initialState
  let snapper = refSnapper reducer ref av
  snap snapper (contraHoistVoid nat component) (reactTarget elm av)

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

runApp :: forall r a. { delayTime :: Number | r } -> ReaderT { delayTime :: Number | r } Aff a -> Effect Unit
runApp r = launchAff_ <<< flip runReaderT r

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  let d = { delayTime : 1000.0 }
  case container of
    Just e  -> runApp d $ reactSnap (runApp d) rootComponent rootReducer { counter: 0, echoer: "", delayer: Just "Clicky the button" } e
    Nothing -> error "no"