module Examples.Routing.UI where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (un)
import Data.Profunctor.Traverse (sequenceDemux)
import Data.Record.Choose (choose)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, expand, inj)
import Effect.Aff (Aff, launchAff_)
import Examples.CatTron.UI (app) as CatTron
import Examples.Reducer.UI (app) as Reducer
import Examples.Routing.Router (urlFor)
import Examples.Routing.Router as Router
import Examples.Routing.State (PageState, PageUpdate, State, Update)
import Examples.Routing.State.Types (Route)
import Examples.TodoMVC.UI (app) as TodoMvc
import Examples.TransactionalForm.UI (app) as Transactional
import React.Basic (JSX)
import React.Basic.DOM as R
import Record (delete)
import Routing.Duplex.Parser (RouteError)
import Snap.Component (ρ)
import Snap.Component.SYTC (Cmp, contraHoist, (||))
import Snap.Component.SYTC as C
import Snap.React.Component ((|-), (|<), (|=))

links :: JSX
links = R.ul |< map toLink ls
  where
  toLink (n /\ a) = R.li |- R.a |= { href: a } |- R.text n
  ls =
    [ "CatTron 9000 Cat Gif Viewing Device" /\ urlFor Router.CatTron
    , "Reducers"                            /\ urlFor Router.Reducer
    , "TodoMVC"                             /\ urlFor Router.TodoMvc
    , "Transactional Forms"                 /\ urlFor Router.Transactional
    ]

root :: forall m s u. Cmp m JSX s u
root _ _ =
  R.div
  |< [ R.h1
       |- R.text "Examples"
     , links
     ]

-- TODO: Make this redirect after a bit
_404 :: Cmp Aff (Aff JSX) RouteError Route
_404 put err = pure $ R.text $ "Invalid route: \"" <> show err <> "\". Redirecting in 5 seconds..."

page :: Cmp Aff (Aff JSX) (Variant (PageState ())) (Variant (PageUpdate ()))
page = un ρ $ sequenceDemux
  { root:     ρ $ root              # C.map pure
  , todomvc:  ρ $ TodoMvc.app       # C.map pure # contraHoist launchAff_
  , cattron:  ρ $ CatTron.app                    # contraHoist launchAff_
  , transact: ρ $ Transactional.app # C.map pure # contraHoist launchAff_
  , reducer:  ρ $ Reducer.app       # C.map pure
  }

app :: Cmp Aff (Aff JSX) (Record State) (Variant Update)
app = C.dimap f g $ _404 || page
  where
  f   { nav: Left err }    = Left err
  f x@{ nav: Right route } = Right $ choose route $ delete (SProxy :: _ "nav") x

  g (Left route) = inj (SProxy :: _ "nav") route
  g (Right x)    = expand x
