module Examples.Routing.UI where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Examples.CatTron.UI as CatTron
import Examples.Reducer.UI as Reducer
import Examples.Routing.Router (urlFor)
import Examples.Routing.Router as Router
import Examples.Routing.State (Action(..), RouteState(..), State)
import Examples.TodoMVC.UI as TodoMvc
import Examples.TransactionalForm.UI as Transactional
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.Component.SYTC (Cmp, contraHoist)
import Snap.Component.SYTC as C
import Snap.React.Component ((|-), (|<), (|=))

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

root :: forall m s u. Cmp m JSX s u
root _ _ = R.div |< [ R.h1 |- R.text "Examples", links ]

app :: Cmp Aff (Aff JSX) State Action
app put s = case s of
  Root            -> pure $                                    root      put unit
  TodoMvc x       -> pure $ handleEffCmp TodoMvc       TodoMvc.app       put x
  CatTron x       ->        handleEffCmp CatTron       CatTron.app       put x
  Transactional x -> pure $ handleEffCmp Transactional Transactional.app put x
  Reducer x       -> pure $ Reducer.app (put <<< ReducerAction) x

handleEffCmp :: forall u s v. (u -> RouteState) -> Cmp Effect v s u -> Cmp Aff v s Action
handleEffCmp st = contraHoist launchAff_ <<< C.rmap (Update <<< st)
