module Examples.Routing.UI where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import Snap.SYTC.Component (Cmp')

app :: Cmp' Effect JSX Unit
app _ _ = R.text "foo"