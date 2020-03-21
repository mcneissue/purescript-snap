module Snap (module Snap, module C, module S, module T) where

import Prelude

import Snap.Component as C
import Snap.Snapper as S
import Snap.Target as T

import Snap.Component.SYTC (Cmp)
import Snap.Snapper (Snapper(..))

encapsulate :: forall m v s u. Monad m => Snapper m u s -> Cmp m v s u -> Cmp m (m v) Unit Void
encapsulate (Snapper { get, put }) cmp _ _ = get <#> cmp put
