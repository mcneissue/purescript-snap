module Data.Record.Extras where

import Prim.Row (class Lacks, class Cons)
import Data.Symbol (SProxy, class IsSymbol)
import Record (insert)

singleton :: forall s v r.
  IsSymbol s =>
  Lacks s () =>
  Cons s v () r =>
  SProxy s -> v -> { | r }
singleton s v = insert s v {}
