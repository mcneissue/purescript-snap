module Snap (module Snap, module C, module S, module T) where

import Prelude

import Snap.Component as C
import Snap.Snapper as S
import Snap.Target as T

import Snap.Component.SYTC (Cmp)
import Snap.Snapper (Snapper(..))
import Snap.Target (Target(..))

encapsulate :: forall m v s u x y. Functor m => Snapper m u s -> Cmp m v s u -> Cmp m (m v) x y
encapsulate (Snapper { get, put }) cmp _ _ = get <#> cmp put

snap :: forall m v x
      . Monad m
     => Cmp m v Unit Void
     -> Target m v
     -> m x
snap cmp t = loop t
  where
  v = cmp absurd unit
  loop (Target render) = do
    t' <- render v
    loop t'
