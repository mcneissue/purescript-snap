module Data.Profunctor.Optics where

import Prelude

import Control.Monad.State (evalState, get, put)
import Data.Array (zipWith)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap, lmap) as B
import Data.Either (Either(..), either)
import Data.Filterable (class Filterable, filter)
import Data.Foldable (class Foldable, and, length)
import Data.Lens (Lens, Lens', Optic', first, left, lens, set, view)
import Data.List (List(..), catMaybes)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Profunctor (class Profunctor, dimap, lcmap, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Lazy (class Lazy2, defer2)
import Data.Profunctor.Monoidal (class Monoidal, class MonoidalMono, class Semigroupal, class SemigroupalMono, zip, zipMono)
import Data.Profunctor.Strong (class Strong)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, swap, uncurry)
import Data.Witherable (class Witherable, wither)
import Effect.Exception.Unsafe (unsafeThrow)

-- Ok, so we want to show that "monomorphic" traversals are equivalent to profunctor
-- optics with the MonoidalMono constraint. If we can show that, it proves that with
-- a little bit of legwork, the stuff in the existing lens library can be generalized
-- so that we can just take any Traversal' and apply it to an arbitrary component.

-- For example if you have a `traverseArray :: forall a. Traversal' [a] a`, you should
-- be able to take some `todo :: Component' m JSX Todo` and just do:
-- ```
-- todos :: Component m JSX [Todo]
-- todos = traverseArray todo
-- ```

-- Now we're going to do this in "easy mode", i.e. we're going to ignore the fact that all
-- the actual traversals in the lens library are in terms of `Wander` and not in terms of
-- `Monoidal` at all. There's some relationship between the two it seems like, but precisely
-- what it is is an open research problem.
--
-- We just ignore this bullshit and assume all the profunctor traversals are going to be written
-- using the appropriate `Monoidal`-like constraints and all the concrete traversals are going
-- to be written using the `LTraversal` thing below (instead of the `Star a b -> Star s t` form
-- it takes in the Haskell lens library).
--
-- The idea is that we're going to relate `MonoidalMono` and `LTraversalMono` (and show that there's
-- a bunch of useful `LTraversalMono`s that we can write, which in `MonoidalMono` form conveniently
-- interact with our components. Then we just do a handwave and say there's *some* analogous relationship
-- between a monomorphized `Traversal` from the Haskell lens library and some specialization of `Wander`
-- like `WanderMono` or something. Those are what you'd want to use for efficiency, figuring those out
-- is left as a future exercise.
--
-- Presumably profunctors for which we can implement `MonoidalMono` also support this hypothetical
-- `WanderMono` (specifically our components do hopefully), although what the relationship between
-- `MonoidalMono` and `WanderMono` is remains a mystery until we know what the relationship between
-- `Monoidal` and `Wander` is.

-- TODO: Move this somewhere else

-- Concrete traversals (a substitute for the Haskell library version of concrete traversals)
type LTraversal s t a b = { contents :: s -> L.List a, fill :: L.List b -> s -> t }

-- Profunctor traversals
type PTraversal s t a b = forall p. Monoidal p => Lazy2 p => p a b -> p s t -- We need the lazy because recursing in purescript is dumb

-- Specialization of concrete traversals where t = s and b = a
type LMonoTraversal s a = LTraversal s s a a

-- Specialization of profunctor traversals where t = s and b = a
type PMonoTraversal s a = forall p. MonoidalMono p => Lazy2 p => p a a -> p s s -- Ditto re: lazy

-- A good intuition for the concrete traversal presentation above is
-- to note that it's isomorphic to a function `s -> FunList a b t`,
-- where `FunList` is the type elaborated below. We're not going to
-- actually work with the `FunList` representation, but it's good to
-- see a slightly different equivalent presentation of the concrete
-- traversals to get a feel for what they're actually doing.

-- It's also important to work through this isomorphism because in the
-- profunctor optics paper they actually just use `s -> FunList a b t`
-- directly as the representation of concrete optics. So while we're
-- using a slightly different representation, running the concrete optic
-- end of our isomorphisms between concrete and profunctor optics through
-- this isomorphism should give us the concrete <-> profunctor isomorphisms
-- that the paper has.

-- This paper btw: http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf

-- A `FunList` consists of some number of `a` values, and a function
-- that converts an equal number of `b` values to a `t`
--
-- | NB:
-- | Really we need typed vectors here, i.e. the representation should be something like:
-- |
-- | ```
-- | data FunList a b t where
-- |   FunList :: forall (n :: Nat). Vec n a -> (Vec n b -> t) -> FunList a b t
-- | ```
-- |
-- | but I don't feel like fighting the PureScript compiler over this, so fuck it.
--
data FunList a b t = FunList (L.List a) (L.List b -> t)

runFunList :: forall a b t x. FunList a b t -> (L.List a -> (L.List b -> t) -> x) -> x
runFunList (FunList i o) f = f i o

-- `FunList`s are applicatives, here's a bunch of shit explaining how:
instance functorFunList :: Functor (FunList a b) where
  map f (FunList i o) = FunList i (map f o)

instance applyFunList :: Apply (FunList a b) where
  apply (FunList i1 ost) (FunList i2 os) = FunList (i1 <> i2) (unsafeSplit (L.length i1) >>> uncurry (\b1 b2 -> ost b1 $ os b2))
    where
    -- #YOLO
    unsafeSplit :: forall x. Int -> L.List x -> Tuple (L.List x) (L.List x)
    unsafeSplit 0 l = Tuple L.Nil l
    unsafeSplit i l = Tuple (L.take i l) (L.drop i l)

instance applicativeFunList :: Applicative (FunList a b) where
  pure x = FunList L.Nil (const x)

-- The concrete traversal presentation we're using above is isomorphic to
-- a function `s -> FunList a b t`.
lt2fl :: forall a b s t. LTraversal s t a b -> (s -> FunList a b t)
lt2fl { contents, fill } s = FunList (contents s) (flip fill s)

fl2lt :: forall a b s t. (s -> FunList a b t) -> LTraversal s t a b
fl2lt f = { contents, fill }
  where
  contents s = runFunList (f s) const
  fill bs s = runFunList (f s) (\_ o -> o bs)

-- Ok, so now we try to prove that there are isomorphisms between:
-- 1. `LTraversal s t a b` <-> `PTraversal s t a b`
-- 2. `LTraversalMono s a` <-> `PTraversalMono s a`

-- The second one is the only one that's applicable for our poor `Component`s, because
-- they can't implement the first one. It's good to show the isomorphism for both the
-- general version and the specialized version though.

-- Alright, here's the forward direction for 1...
lt2pt :: forall s t a b. LTraversal s t a b -> PTraversal s t a b
lt2pt { contents, fill } = rec >>> lcmap contents >>> first >>> dimap dup (uncurry fill)
  where
  rec h = zip h (defer2 $ \_ -> rec h) # rmap cons # left # dimap uncons merge
  uncons L.Nil         = Right L.Nil
  uncons (L.Cons a as) = Left (Tuple a as)
  cons = uncurry L.Cons
  dup a = Tuple a a
  merge = either identity identity

-- And the forward direction for 2...
--
-- | NB:
-- | This is basically an exact duplicate of the definition above, the only thing that is
-- | different is that we're using the weaker MonoidalMono constraint, and therefore use
-- | zipMono in the definition below. That also demands that the types be less flexible;
-- | we only have two degrees of freedom now.
-- |
-- | There has *got* to be a way to do this less shittily, but for now copy paste works,
-- | brain not work good at 4 AM
--
lmt2pmt :: forall s a. LMonoTraversal s a -> PMonoTraversal s a
lmt2pmt { contents, fill } = rec >>> lcmap contents >>> first >>> dimap dup (uncurry fill)
  where
  rec h = zipMono h (defer2 $ \_ -> rec h) # rmap cons # left # dimap uncons merge
  uncons L.Nil = Right L.Nil
  uncons (L.Cons a as) = Left (Tuple a as)
  cons = uncurry L.Cons
  dup a = Tuple a a
  merge = either identity identity

-- The backwards direction is a little bit tricker. The technique here is to note that the
-- concrete traversal type `LTraversal` actually forms a profunctor itself, and we can exploit
-- this fact to write the half of the isomorphism that turns a concrete traversal into a
-- constrained profunctor transformation.

-- Here's a newtype wrapper to bring the profunctorial parameters around into the right position
newtype LTraversal' a b s t = LTraversal' (LTraversal s t a b)

runLTraversal' :: forall s t a b. LTraversal' a b s t -> LTraversal s t a b
runLTraversal' (LTraversal' l) = l

instance lazy2Traversal' :: Lazy2 (LTraversal' a b) where
  defer2 f = LTraversal' { contents, fill }
    where
    contents s = let l = runLTraversal' (f unit) in l.contents s
    fill xs s = let l = runLTraversal' (f unit) in l.fill xs s

-- Ok, here's all the instances, all the way from `Profunctor` to `Monoidal`
instance profunctorTraversal :: Profunctor (LTraversal' a b) where
  dimap f g (LTraversal' l) = LTraversal' { contents, fill }
    where
    contents = lcmap f l.contents
    fill = dimap f g <<< l.fill

firstTraversal :: forall a b s t x. LTraversal' a b s t -> LTraversal' a b (Tuple s x) (Tuple t x)
firstTraversal (LTraversal' l) = LTraversal' { contents, fill }
  where
  contents = lcmap (uncurry const) l.contents
  fill = B.lmap <<< l.fill

instance strongTraversal :: Strong (LTraversal' a b) where
  first = firstTraversal
  second = dimap swap swap <<< firstTraversal

leftTraversal :: forall a b s t x. LTraversal' a b s t -> LTraversal' a b (Either s x) (Either t x)
leftTraversal (LTraversal' l) = LTraversal' { contents, fill }
  where
  contents = either l.contents (const L.Nil)
  fill xs = B.lmap (l.fill xs)

flipEither :: forall a b. Either a b -> Either b a
flipEither (Left x) = Right x
flipEither (Right x) = Left x

instance choiceTraversal :: Choice (LTraversal' a b) where
  left = leftTraversal
  right = dimap flipEither flipEither <<< leftTraversal

instance semigroupalMonoTraversal :: SemigroupalMono (LTraversal' a b) where
  zipMono = zip

instance semigroupalTraversal :: Semigroupal (LTraversal' a b) where
  zip (LTraversal' l) (LTraversal' m) = LTraversal' { contents, fill }
    where
    contents = bifoldMap l.contents m.contents
    fill xs = B.bimap (l.fill xs) (m.fill xs)

instance monoidalMonoTraversal :: MonoidalMono (LTraversal' a b) where
  infinite = LTraversal' { contents, fill }
    where
    contents = const L.Nil
    fill = flip const

instance monoidalTraversal :: Monoidal (LTraversal' a b)

-- We're going to be treating lists as sized vectors, going to have
-- to do stuff like this
unsafeUncons :: forall x. L.List x -> Tuple x (L.List x)
unsafeUncons (L.Nil)       = unsafeThrow "List can't be empty!"
unsafeUncons (L.Cons a as) = Tuple a as

unsafeHead :: forall x. L.List x -> x
unsafeHead = fst <<< unsafeUncons

-- And finally, the reverse morphism for 1...
pt2lt :: forall s t a b. PTraversal s t a b -> LTraversal s t a b
pt2lt f = runLTraversal' $ f $ LTraversal' single
  where
  -- Ideally we'd have type information that witnesses `pure` produces a one element list but whatever
  single =
    { contents: pure
    , fill: const <<< unsafeHead
    }

-- Again we repeat all the bullshit to get the reverse direction for 2...
pmt2lmt :: forall s a. PMonoTraversal s a -> LMonoTraversal s a
pmt2lmt f = runLTraversal' $ f $ LTraversal' single
  where
  single =
    { contents: pure
    , fill: case _ of
            Nil -> identity
            Cons x _ -> const x
    }

-- Et voila! Now we can write a little traversal in concrete form, transform it to a profunctor traversal,
-- and apply it straight to a component!

-- Fill a traversable using a list containing at least as many elements
-- Simply explodes if the list contains less elements than the traversable
unsafeFillTraversable :: forall t a b. Traversable t => L.List b -> t a -> t b
unsafeFillTraversable l = flip evalState l <<< traverse (const step)
  where
  step = do
    (Tuple x xs) <- unsafeUncons <$> get
    put xs
    pure x

traversed :: forall t a b. Traversable t => PTraversal (t a) (t b) a b
traversed = lt2pt
  { contents: L.fromFoldable
  -- Safe to use unsafeFillTraversable here, list is created from the traversable
  , fill: unsafeFillTraversable
  }

traversed' :: forall t a. Traversable t => PMonoTraversal (t a) a
traversed' = lmt2pmt
  { contents: L.fromFoldable
  -- Safe to use unsafeFillTraversable here, list is created from the traversable
  , fill: unsafeFillTraversable
  }


-- Similar to unsafeFillTraversable, but list can be smaller
overwriteWitherable :: forall t a b. Witherable t => L.List b -> t a -> t b
overwriteWitherable l = flip evalState l <<< wither step
  where
  step v = do
    xs <- get
    case xs of
      L.Nil        -> pure Nothing
      L.Cons x xs' -> Just x <$ put xs'

iterated :: forall t a b. Witherable t => PTraversal (t a) (t b) a b
iterated = lt2pt
  { contents: L.fromFoldable
  , fill: overwriteWitherable
  }


iterated' :: forall t a. Witherable t => PMonoTraversal (t a) a
iterated' = lmt2pmt
  { contents: L.fromFoldable
  , fill: overwriteWitherable
  }

partsOf :: forall s t a. PTraversal s t a a -> Lens s t (L.List a) (L.List a)
partsOf t = lens contents (flip fill)
  where
  { contents, fill } = pt2lt t

partsOf' :: forall s a. PMonoTraversal s a -> Lens' s (L.List a)
partsOf' t = lens contents (flip fill)
  where
  { contents, fill } = pmt2lmt t

withered :: forall t a b. Witherable t => PTraversal (t a) (t b) a (Maybe b)
withered = lt2pt { contents, fill }
  where
  contents = L.fromFoldable
  fill = catMaybes >>> overwriteWitherable

withered' :: forall t a. Witherable t => PMonoTraversal (t a) (Maybe a)
withered' = lmt2pmt { contents, fill }
  where
  contents = L.fromFoldable <<< map Just
  fill = catMaybes >>> overwriteWitherable

by :: forall p a. Profunctor p => (a -> Boolean) -> Optic' p a (Either a a)
by f = dimap (\v -> if f v then Left v else Right v) (either identity identity)

all :: forall t a. Functor t => Foldable t => HeytingAlgebra a => Eq a => Lens' (t a) a
all = lens and (\s b -> if and s == b then s else b <$ s)

overArray :: forall s t a b. Lens s t a b -> Lens (Array s) (Array t) (Array a) (Array b)
overArray l = lens (map $ view l) (zipWith $ flip (set l))

type Getter s a = forall p x. Profunctor p => p a x -> p s x

countBy :: forall f x. Filterable f => Foldable f => (x -> Boolean) -> Getter (f x) Int
countBy p = lcmap (filter p >>> length)

data Edit s = Change s | Save | Revert
type Editable s = { saved :: s, modified :: Maybe s }

edited :: forall s. Lens (Editable s) (Editable s) s (Edit s)
edited = lens view (flip update)
  where
  view { saved, modified } = fromMaybe saved modified
  update (Change v)   { saved, modified } = { saved, modified: Just v }
  update Revert       { saved, modified } = { saved, modified: Nothing }
  update Save       s@{ saved, modified } = { saved: view s, modified: Nothing }

isEditing :: forall s. Lens' (Editable s) Boolean
isEditing = lens view (flip update)
  where
  view { modified } = isJust modified
  update true { saved, modified } = { saved, modified: Just $ fromMaybe saved modified }
  update false { saved, modified } = { saved, modified: Nothing }
