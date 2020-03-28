module Examples.TransactionalForm.UI where

import Prelude

import Data.Functor.Variant (SProxy(..))
import Data.Lens.Record (prop)
import Effect (Effect)
import Examples.TransactionalForm.State (FormData, State)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.Component ((#!))
import Snap.React.Component ((|<), (|~))
import Snap.React.Component as S
import Snap.Component.SYTC (Cmp')
import Snap.Component.SYTC as C

form :: Cmp' Effect JSX FormData
form = C.ado
  e <- S.edited  #! prop (SProxy :: _ "name")
  c <- S.counter #! prop (SProxy :: _ "age")
  in R.div
     |< [ R.input |~ e $ {}
        , c
        ]

app :: Cmp' Effect JSX State
app = C.ado
  t <- S.transacted
       { change: form
       , revert: S.button
       , save:   S.button
       }
  in t.change
  <> t.revert
     |< [ R.text "Revert"
        ]
  <> t.save
     |< [ R.text "Save"
        ]
