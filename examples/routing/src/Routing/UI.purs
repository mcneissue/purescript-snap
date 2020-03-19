module Examples.Routing.UI where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Examples.CatTron.UI as CatTron
import Examples.Reducer.UI as Reducer
import Examples.Routing.Router (mkRouter, urlFor)
import Examples.Routing.Router as Router
import Examples.Routing.State (Action(..), RouteState(..), State)
import Examples.TodoMVC.UI as TodoMvc
import Examples.TransactionalForm.UI as Transactional
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.React.Component ((|-), (|<), (|=), (|~))
import Snap.Component.SYTC (Cmp, contraHoist)

links :: JSX
links = R.ul |< map toLink ls
  where
  toLink (Tuple n a) = R.li |- R.a |= { href: a } |- R.text n
  ls =
    [ Tuple "CatTron 9000 Cat Gif Viewing Device" $ urlFor Router.CatTron
    , Tuple "Reducers" $ urlFor Router.Reducer
    , Tuple "TodoMVC" $ urlFor Router.TodoMvc
    , Tuple "Transactional Forms" $ urlFor Router.Transactional
    ]

app :: Cmp Aff (Aff JSX) State Action
app put s = case s of
  Root -> pure $ R.div |< [ R.h1 |- R.text "Examples", links ]
  TodoMvc x -> pure $ handleEffCmp put TodoMvc.app TodoMvc x
  CatTron x -> handleEffCmp put CatTron.app CatTron x
  Reducer x -> pure $ Reducer.app (put <<< ReducerAction) x
  Transactional x -> pure $ handleEffCmp put Transactional.app Transactional x

handleEffCmp :: forall u s v. (Action -> Aff Unit) -> Cmp Effect v s u -> (u -> RouteState) -> s -> v
handleEffCmp put cmp st x = contraHoist launchAff_ cmp (put <<< Update <<< st) x
