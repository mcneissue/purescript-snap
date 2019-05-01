module Main where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Effect.Class (class MonadEffect, liftEffect)

import Data.Lens (Lens, over)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref (Ref, modify, modify_, new, read)

import React.Basic (Component, JSX, createComponent, make) as R
import React.Basic.DOM (button, div, render, text) as R
import React.Basic.DOM.Events as RE
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

newtype Target m v = Target (Maybe (v -> m (Target m v)))

newtype Component m v s u = Component ((u -> m Unit) -> s -> v)

runComponent :: forall m v s u. Component m v s u -> ((u -> m Unit) -> s -> v)
runComponent (Component cmp) = cmp

instance profunctorComponent :: Profunctor (Component m v) where
  dimap :: forall a b c d m v. (a -> b) -> (c -> d) -> Component m v b c -> Component m v a d
  dimap f g (Component cmp) = Component \u s -> cmp (u <<< g) (f s)

instance strongComponent :: Strong (Component m v) where
  first (Component cmp)  = Component \u s -> cmp (u <<< (\b -> Tuple b (snd s))) (fst s)
  second (Component cmp) = Component \u s -> cmp (u <<< (\b -> Tuple (fst s) b)) (snd s)

instance choiceComponent :: Monoid v => Choice (Component m v) where
  left (Component cmp) = Component \u -> either (cmp (u <<< Left)) (const mempty)
  right (Component cmp) = Component \u -> either (const mempty) (cmp (u <<< Right))

refocus :: forall m s u v u' s'. Lens s' u' s u -> Component m v s u -> Component m v s' u'
refocus lens = lens

contraHoist :: forall m m' v s u. (m' ~> m) -> Component m v s u -> Component m' v s u
contraHoist nat (Component cmp) = Component \u s -> cmp (nat <<< u) s

data Action = Increment | Decrement

counter :: Component Effect R.JSX Int Action
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

counter' :: forall r
          . Ref { state :: Int | r } 
         -> Component (ReaderT (Ref { state :: Int | r }) Effect) R.JSX Int Action
counter' r = Component \update s -> 
  runComponent (contraHoist (flip runReaderT r) counter) (\u -> handler u *> update u) s

handler :: forall r m. MonadEffect m => MonadReader (Ref { state :: Int | r}) m => Action -> m Unit
handler Increment = modifyState (add 1)
handler Decrement = modifyState (\s -> s - 1)

modifyState :: forall m a r. MonadEffect m => MonadReader (Ref { state :: a | r}) m => (a -> a) -> m Unit
modifyState f = ask >>= liftEffect <<< modify_ (over state f)

snap :: forall m v s u r
      . MonadEffect m 
     => MonadReader (Ref { state :: s | r }) m 
     => Component m v s u 
     -> Target m v 
     -> m Unit
snap (Component cmp) t = loop t
  where
    loop tar@(Target t') = case t' of
      Just render -> do
        s <- ask >>= liftEffect <<< map _.state <<< read
        let v = cmp (const $ loop tar) s
        _ <- render v -- where should the initial render go?
        pure unit
      Nothing -> pure unit

state = prop (SProxy :: SProxy "state")

action = prop (SProxy :: SProxy "action")

putUpdate :: forall m s u r. MonadEffect m => MonadReader (Ref { state :: s, action :: Maybe u | r }) m => Maybe u -> m Unit
putUpdate u = void $ ask >>= liftEffect <<< modify (over action $ const u)

reactTarget :: forall m. MonadEffect m => Element -> Target m R.JSX
reactTarget e = Target $ Just go
  where
    go v = do
      liftEffect $ log "here"
      liftEffect $ R.render v e
      pure $ Target $ Just go

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  s <- new { state: 0, action: Nothing }
  case container of
    Just e  -> runReaderT (snap (counter' s) (reactTarget e)) s
    Nothing -> error "no"
