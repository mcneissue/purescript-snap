module Snap.Component where

import Prelude

import Data.Newtype (class Newtype, un, under)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Lazy (class Lazy2)
import Data.Profunctor.Monoidal (class Comonoidal, class ComonoidalMono, class Cosemigroupal, class CosemigroupalMono, class MonoidalMono, class SemigroupalMono, switch)
import Data.Profunctor.Strong (class Strong)
import Snap.SYTC.Component (Cmp)
import Snap.SYTC.Component as C

-- Profunctor wrapper
newtype PComponent m v s u
  = PComponent (Cmp m v s u)

derive instance newtypeComponent :: Newtype (PComponent m v s u) _

ρ :: forall m v s u. Cmp m v s u -> PComponent m v s u
ρ = PComponent

type PComponent' m v s
  = PComponent m v s s

instance componentLazy :: Lazy2 (PComponent m v) where
  defer2 f = ρ $ actual (un ρ <<< f)
    where
    actual = C.defer

instance semigroupComponent :: Semigroup v => Semigroup (PComponent m v s u) where
  append a b = ρ $ actual (un ρ a) (un ρ b)
    where
    actual = C.append

instance monoidComponent :: Monoid v => Monoid (PComponent m v s u) where
  mempty = ρ actual
    where
    actual = C.mempty

instance profunctorComponent :: Profunctor (PComponent m v) where
  dimap f g cmp = ρ $ actual f g (un ρ cmp)
    where
    actual = C.dimap

instance strongComponent :: Strong (PComponent m v) where
  first = ρ <<< actual <<< un ρ
    where
    actual = C.first
  second = ρ <<< actual <<< un ρ
    where
    actual = C.second

instance choiceComponent :: Monoid v => Choice (PComponent m v) where
  left = ρ <<< actual <<< un ρ
    where
    actual = C.left
  right = ρ <<< actual <<< un ρ
    where
    actual = C.right

instance cosemigroupalMonoComponent :: CosemigroupalMono (PComponent m v) where
  switchMono = switch

instance cosemigroupalComponent :: Cosemigroupal (PComponent m v) where
  switch f g = ρ $ actual (un ρ f) (un ρ g)
    where
    actual = C.switch

instance comonoidalMonoComponent :: ComonoidalMono (PComponent m v) where
  never = ρ actual
    where
    actual = C.never

instance comonoidalComponent :: Comonoidal (PComponent m v)

instance semigroupalMonoComponent :: Monoid v => SemigroupalMono (PComponent m v) where
  zipMono f g = ρ $ actual (un ρ f) (un ρ g)
    where
    actual = C.zipMono

instance monoidalMonoComponent :: Monoid v => MonoidalMono (PComponent m v) where
  infinite = ρ actual
    where
    actual = C.infinite

focus :: forall m v s u x y. Newtype x y => (PComponent m v s u -> x) -> Cmp m v s u -> y
focus = under ρ

infixl 8 focus as $!

flippedFocus :: forall m v s u x y. Newtype x y => Cmp m v s u -> (PComponent m v s u -> x) -> y
flippedFocus = flip focus

infixr 8 flippedFocus as #!

-- Monad wrapper
newtype MComponent s u m v
  = MComponent (Cmp m v s u)

derive instance newtypeMComponent :: Newtype (MComponent s u m v) _

μ :: forall s u m v. Cmp m v s u -> MComponent s u m v
μ = MComponent

instance functorMComponent :: Functor (MComponent s u m) where
  map f = μ <<< actual f <<< un μ
    where
    actual = C.map

instance applyMComponent :: Apply (MComponent s u m) where
  apply fab fa = μ $ un μ fab `actual` un μ fa
    where
    actual = C.apply

instance bindMComponent :: Bind (MComponent s u m) where
  bind ma amb = μ $ (un μ ma) `actual` (amb >>> un μ)
    where
    actual = C.bind

instance applicativeMComponent :: Applicative (MComponent s u m) where
  pure = μ <<< actual
    where
    actual = C.pure

newtype CComponent m s u c a b
  = CComponent (Cmp m (c a b) s u)

derive instance newtypeCComponent :: Newtype (CComponent m s u c a b) _

-- Category wrapper
κ :: forall m s u c a b. Cmp m (c a b) s u -> CComponent m s u c a b
κ = CComponent

instance semigroupoidCComponent :: Semigroupoid c => Semigroupoid (CComponent m s u c) where
  compose bc ab = κ $ actual (un κ bc) (un κ ab)
    where
    actual = C.compose

instance categoryMComponent :: Category c => Category (CComponent m u s c) where
  identity = κ $ actual
    where
    actual = C.identity
