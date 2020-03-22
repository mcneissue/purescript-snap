module Examples.Routing.UI where

import Prelude

import Data.Lens ((^?))
import Data.Profunctor (rmap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Examples.CatTron.UI as CatTron
import Examples.Reducer.UI as Reducer
import Examples.Routing.Router (urlFor)
import Examples.Routing.Router as Router
import Examples.Routing.State (Action(..), RouteState, State, _cattron, _reducer, _root, _todomvc, _transactional)
import Examples.TodoMVC.UI as TodoMvc
import Examples.TransactionalForm.UI as Transactional
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap ((#!))
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
root _ _ =
  R.div
  |< [ R.h1
       |- R.text "Examples"
     , links
     ]

app :: Cmp Aff (Aff JSX) State Action
app = C.ado
  r  <- root              #! _root                        #! rmap Update
  t  <- TodoMvc.app       #! _todomvc                     #! rmap Update        # contraHoist launchAff_
  c  <- CatTron.app       #! _cattron                     #! rmap Update        # contraHoist launchAff_
  tr <- Transactional.app #! _transactional               #! rmap Update        # contraHoist launchAff_
  rd <- Reducer.app       #  C.lcmapMaybe (_ ^? _reducer) #! rmap ReducerAction
  in
  pure r <> pure t <> c <> pure tr <> pure rd
