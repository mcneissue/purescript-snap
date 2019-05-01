module Snap where

import Prelude
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Reader (asks)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, modify_, read)
import Data.Maybe (Maybe(..))
import Data.Lens (Lens', view, over)

import Component (Component(..))

newtype Target m v = Target (Maybe (v -> m (Target m v)))

snap :: forall m v s s' u r
      . MonadEffect m 
     => MonadReader { ref :: Ref s' | r } m 
     => Lens' s' s 
     -> Component m v s u
     -> Target m v 
     -> m Unit
snap l (Component cmp) t = loop t
  where
    loop tar@(Target t') = case t' of
      Just render -> do
        s <- asks _.ref >>= liftEffect <<< map (view l) <<< read
        let v = cmp (const $ loop tar) s
        _ <- render v -- where should the initial render go?
        pure unit
      Nothing -> pure unit

modifyRef :: forall r r' a m
           . MonadReader { ref :: Ref r | r' } m
          => MonadEffect m
          => Lens' r a
          -> (a -> a) 
          -> m Unit
modifyRef l f = asks _.ref >>= liftEffect <<< modify_ (over l f)