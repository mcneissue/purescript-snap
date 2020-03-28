module Data.Profunctor.Optics where

import Prelude

import Control.Monad.State (get, runState)
import Data.Array (zipWith)
import Data.Either (Either(..), either)
import Data.Filterable (class Filterable, filter)
import Data.Foldable (class Foldable, and, length)
import Data.Lens (Iso', Lens, Lens', Traversal, lens, set, traverseOf, view, (^..))
import Data.List ((!!))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.Tuple (fst)

partsOf :: forall s t a. Traversal s t a a -> Lens s t (L.List a) (L.List a)
partsOf t = lens (_ ^.. t) (\s as -> fst $ flip runState 0 $ traverseOf t (go as) s)
  where
  go as a = do
    i <- get
    pure $ case as !! i of
      Nothing -> a
      Just a' -> a'

by :: forall a. (a -> Boolean) -> Iso' a (Either a a)
by f = dimap (\v -> if f v then Left v else Right v) (either identity identity)

all ::
  forall t a.
  Functor t =>
  Foldable t =>
  HeytingAlgebra a =>
  Eq a =>
  Lens' (t a) a
all = lens and (\s b -> if and s == b then s else b <$ s)

overArray :: forall s t a b. Lens s t a b -> Lens (Array s) (Array t) (Array a) (Array b)
overArray l = lens (map $ view l) (zipWith $ flip (set l))

type Getter s a = forall p x. Profunctor p => p a x -> p s x

countBy :: forall f x. Filterable f => Foldable f => (x -> Boolean) -> Getter (f x) Int
countBy p = lcmap (filter p >>> length)

data Edit s = Change s | Save | Revert
type Transactional s = { value :: s, modification :: Maybe s }

atomically :: forall s. Lens (Transactional s) (Transactional s) s (Edit s)
atomically = lens view (flip update)
  where
  view { value, modification } = fromMaybe value modification
  update (Change v)   { value, modification } = { value, modification: Just v }
  update Revert       { value, modification } = { value, modification: Nothing }
  update Save       s@{ value, modification } = { value: view s, modification: Nothing }

isDirty :: forall s. Lens' (Transactional s) Boolean
isDirty = lens view (flip update)
  where
  view { modification } = isJust modification
  update true  { value, modification } = { value, modification: Just $ fromMaybe value modification }
  update false { value, modification } = { value, modification: Nothing }
