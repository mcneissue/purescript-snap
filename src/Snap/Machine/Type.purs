module Snap.Machine.Type where

import Snap.Machine.Step (Step)

type Coalgebra f a = a -> f a

type Machine s i o = Coalgebra (Step i o) s