module Examples.Routing.UI where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/), in1, in2, in3, in4)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), get1, get2, get3, get4, (/\))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Examples.CatTron.State (State) as CatTron
import Examples.CatTron.UI (app) as CatTron
import Examples.Reducer.State (Action, State) as Reducer
import Examples.Reducer.UI (app) as Reducer
import Examples.Routing.Router (urlFor)
import Examples.Routing.Router as Router
import Examples.TodoMVC.State (App) as TodoMvc
import Examples.TodoMVC.UI (app) as TodoMvc
import Examples.TransactionalForm.State (State) as Transactional
import Examples.TransactionalForm.UI (app) as Transactional
import React.Basic (JSX)
import React.Basic.DOM as R
import Routing.Duplex.Parser (RouteError)
import Snap.Component.SYTC (Cmp, contraHoist, (||))
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

-- TODO: Make this redirect after a bit
_404 :: forall m. MonadAff m => Cmp m (m JSX) RouteError Router.Route
_404 put err = pure $ R.text $ "Invalid route: \"" <> show err <> "\". Redirecting in 5 seconds..."

type S = Unit \/ TodoMvc.App \/ CatTron.State \/ Transactional.State \/ Reducer.State
type U = Void \/ TodoMvc.App \/ CatTron.State \/ Transactional.State \/ Reducer.Action

page :: Cmp Aff (Aff JSX) S U
page =
  C.map pure root
  || contraHoist launchAff_ (C.map pure TodoMvc.app)
  || contraHoist launchAff_ CatTron.app
  || contraHoist launchAff_ (C.map pure Transactional.app)
  || C.map pure Reducer.app

type S' = Unit /\ TodoMvc.App /\ CatTron.State /\ Transactional.State /\ Reducer.State

app ::
  Cmp Aff (Aff JSX)
  ((RouteError \/ Router.Route) /\ S')
  (Router.Route                 \/ U)
app put (Left err                   /\ _) = _404 (put <<< Left) err
app put (Right Router.Root          /\ s) = page (put <<< Right) (in1 $ get1 $ s)
app put (Right Router.TodoMvc       /\ s) = page (put <<< Right) (in2 $ get2 $ s)
app put (Right Router.CatTron       /\ s) = page (put <<< Right) (in3 $ get3 $ s)
app put (Right Router.Transactional /\ s) = page (put <<< Right) (in4 $ get4 $ s)
app put (Right Router.Reducer       /\ _ /\ _ /\ _ /\ _ /\ s) = page (put <<< Right) (Right $ Right $ Right $ Right $ s)
