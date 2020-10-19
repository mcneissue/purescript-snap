module Snap.Component where

import Prelude

import Data.Either (Either)
import Data.Newtype (class Newtype, un, under)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Lazy (class Lazy)
import Data.Profunctor.Monoidal (class Monoidal, class Semigroupal, class Unital)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Snap.Component.SYTC (Component)
import Snap.Component.SYTC as C

-- Profunctor wrapper
newtype PComponent m v b s u
  = PComponent (Component m v b s u)

derive instance newtypeComponent :: Newtype (PComponent m v b s u) _

ρ :: forall m v b s u. Component m v b s u -> PComponent m v b s u
ρ = PComponent

type PComponent' m v s
  = PComponent m v s s

instance componentLazy :: Lazy (PComponent m v b) where
  defer f = ρ $ actual (un ρ <<< f)
    where
    actual = C.defer

instance semigroupComponent :: Semigroup v => Semigroup (PComponent m v b s u) where
  append a b = ρ $ actual (un ρ a) (un ρ b)
    where
    actual = C.append

instance monoidComponent :: Monoid v => Monoid (PComponent m v b s u) where
  mempty = ρ actual
    where
    actual = C.mempty

instance profunctorComponent :: Profunctor (PComponent m v b) where
  dimap f g cmp = ρ $ actual f g (un ρ cmp)
    where
    actual = C.dimap

instance strongComponent :: Strong (PComponent m v b) where
  first = ρ <<< actual <<< un ρ
    where
    actual = C.first
  second = ρ <<< actual <<< un ρ
    where
    actual = C.second

instance choiceComponent :: Monoid v => Choice (PComponent m v b) where
  left = ρ <<< actual <<< un ρ
    where
    actual = C.left
  right = ρ <<< actual <<< un ρ
    where
    actual = C.right

instance eetSemigroupal :: Semigroupal (->) Either Either Tuple (PComponent m v b) where
  pzip (f /\ g) = ρ $ actual (un ρ f) (un ρ g)
    where
    actual = C.demux

instance eetUnital :: Unital (->) Void Void Unit (PComponent m v b) where
  punit _ = ρ actual
    where
    actual = C.initial

instance eetMonoidal :: Monoidal (->) Either Void Either Void Tuple Unit (PComponent m v b)

instance tetSemigroupal :: Semigroup v => Semigroupal (->) Tuple Either Tuple (PComponent m v b) where
  pzip (f /\ g) = ρ $ actual (un ρ f) (un ρ g)
    where
    actual = C.switch

instance tetUnital :: Monoid v => Unital (->) Unit Void Unit (PComponent m v b) where
  punit _ = ρ actual
    where
    actual = C.poly

instance tetMonoidal :: Monoid v => Monoidal (->) Tuple Unit Either Void Tuple Unit (PComponent m v b)

focus :: forall m v b s u x y. Newtype x y => (PComponent m v b s u -> x) -> Component m v b s u -> y
focus = under ρ

infixl 1 focus as $!

flippedFocus :: forall m v b s u x y. Newtype x y => Component m v b s u -> (PComponent m v b s u -> x) -> y
flippedFocus = flip focus

infixl 1 flippedFocus as #!

-- Monad wrapper
newtype MComponent b s u m v
  = MComponent (Component m v b s u)

derive instance newtypeMComponent :: Newtype (MComponent b s u m v) _

μ :: forall b s u m v. Component m v b s u -> MComponent b s u m v
μ = MComponent

instance functorMComponent :: Functor (MComponent b s u m) where
  map f = μ <<< actual f <<< un μ
    where
    actual = C.map

instance applyMComponent :: Apply (MComponent b s u m) where
  apply fab fa = μ $ un μ fab `actual` un μ fa
    where
    actual = C.apply

instance bindMComponent :: Bind (MComponent b s u m) where
  bind ma amb = μ $ (un μ ma) `actual` (amb >>> un μ)
    where
    actual = C.bind

instance applicativeMComponent :: Applicative (MComponent b s u m) where
  pure = μ <<< actual
    where
    actual = C.pure

newtype CComponent m b s u c x y
  = CComponent (Component m (c x y) b s u)

derive instance newtypeCComponent :: Newtype (CComponent m b s u c x y) _

-- Category wrapper
κ :: forall m b s u c x y. Component m (c x y) b s u -> CComponent m b s u c x y
κ = CComponent

instance semigroupoidCComponent :: Semigroupoid c => Semigroupoid (CComponent m b s u c) where
  compose bc ab = κ $ actual (un κ bc) (un κ ab)
    where
    actual = C.compose

instance categoryMComponent :: Category c => Category (CComponent m b u s c) where
  identity = κ $ actual
    where
    actual = C.identity
