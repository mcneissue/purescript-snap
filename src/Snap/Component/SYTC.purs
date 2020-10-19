module Snap.Component.SYTC where

import Control.Applicative (class Applicative)
import Control.Apply (lift2) as A
import Control.Category (class Category, class Semigroupoid, (<<<), (>>>))
import Data.Array (mapWithIndex)
import Data.Compactable (compact)
import Data.Either (Either(..), either)
import Data.Eq ((==))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, unit, class Semigroup, class Monoid, (<>), ($), (>>=), flip, const, Void, absurd, type (~>))
import Prelude as P

type Cmp m v b s u
  = (u -> m b) -> s -> v

-- TODO Naming
type Cmp' m v s = Cmp m v Unit s s
type Cmp'' m v b s = Cmp m v b s s

-- Lazy2
defer :: forall m v b s u. (Unit -> Cmp m v b s u) -> Cmp m v b s u
defer f set s = f unit set s

-- Semigroup
append :: forall m v b s u. Semigroup v => Cmp m v b s u -> Cmp m v b s u -> Cmp m v b s u
append = (<>)

-- Monoid
mempty :: forall m v b s u. Monoid v => Cmp m v b s u
mempty = P.mempty

-- Profunctor
dimap :: forall m v b x' x y y'. (x' -> x) -> (y -> y') -> Cmp m v b x y -> Cmp m v b x' y'
dimap f g cmp u s = cmp (u <<< g) (f s)

lcmap :: forall m v b s s' u. (s -> s') -> Cmp m v b s' u -> Cmp m v b s u
lcmap f = dimap f P.identity

rmap :: forall m v b s u u'. (u -> u') -> Cmp m v b s u -> Cmp m v b s u'
rmap = dimap P.identity

-- Strong
first :: forall m v b s u c. Cmp m v b s u -> Cmp m v b (Tuple s c) (Tuple u c)
first cmp u s = let c = snd s in cmp (u <<< flip Tuple c) (fst s)

second :: forall m v b s u c. Cmp m v b s u -> Cmp m v b (Tuple c s) (Tuple c u)
second = dimap swap swap <<< first

-- Choice
left :: forall m v b s u c. Monoid v => Cmp m v b s u -> Cmp m v b (Either s c) (Either u c)
left cmp set = either (cmp $ set <<< Left) (const P.mempty)

right :: forall m v b s u c. Monoid v => Cmp m v b s u -> Cmp m v b (Either c s) (Either c u)
right = dimap flipEither flipEither <<< left
  where
  flipEither :: forall x y. Either x y -> Either y x
  flipEither = either Right Left

-- Contrahoistable
contraHoist :: forall v b s u m n. (n ~> m) -> Cmp m v b s u -> Cmp n v b s u
contraHoist f cmp set s = cmp (f <<< set) s

-- TODO What is this?
contraHoist_ :: forall v b s u m n x. (n b -> m x) -> Cmp m v x s u -> Cmp n v b s u
contraHoist_ f cmp set s = cmp (f <<< set) s

-- Functor
map :: forall m b u s x y. (x -> y) -> Cmp m x b s u -> Cmp m y b s u
map = compose2
  where
  compose2 = (<<<) <<< (<<<)

infixl 4 map as <$>!

mapFlipped :: forall m b u s x y. Cmp m x b s u -> (x -> y) -> Cmp m y b s u
mapFlipped = flip map

infixr 4 mapFlipped as <#>!

-- Apply
apply :: forall m b u s x y. Cmp m (x -> y) b s u -> Cmp m x b s u -> Cmp m y b s u
apply = lift2 ($)

infixl 4 apply as <*>!

lift2 :: forall m b s u x y z. (x -> y -> z) -> Cmp m x b s u -> Cmp m y b s u -> Cmp m z b s u
lift2 f c1 c2 = un Compose $ A.lift2 f (Compose c1) (Compose c2)

-- Applicative
pure :: forall m v b s u. v -> Cmp m v b s u
pure v _ _ = v

-- Bind, Monad
-- NB: All this currying/uncurrying nonsense is going on because I can't figure out
--     a way to express (without newtype overhead) that fn2.bind = (readert fn).bind
bind :: forall m b s u x y. Cmp m x b s u -> (x -> Cmp m y b s u) -> Cmp m y b s u
bind c f = curry $ (uncurry c) >>= (f >>> uncurry)

infixl 1 bind as >>=!

-- Semigroupoid
compose :: forall m b s u c x y z. Semigroupoid c => Cmp m (c y z) b s u -> Cmp m (c x y) b s u -> Cmp m (c x z) b s u
compose = lift2 (<<<)

infixl 9 compose as <<<!

composeFlipped :: forall m b s u c x y z. Semigroupoid c => Cmp m (c x y) b s u -> Cmp m (c y z) b s u -> Cmp m (c x z) b s u
composeFlipped = flip compose

infixr 9 composeFlipped as >>>!

-- Category
identity :: forall m b s u c x. Category c => Cmp m (c x x) b s u
identity = pure P.identity

-- Contrafilterable
lcmapMaybe :: forall m v b s s' u. Monoid v => (s -> Maybe s') -> Cmp m v b s' u -> Cmp m v b s u
lcmapMaybe p cmp put s = case p s of
  Nothing -> P.mempty
  Just s' -> cmp put s'

-- Demux
demux :: forall m v b a c x y. Cmp m v b a x -> Cmp m v b c y -> Cmp m v b (Either a c) (Either x y)
demux f g set = either (f $ set <<< Left) (g $ set <<< Right)

infixr 4 demux as ||

-- Demuxative
initial :: forall m v b a. Cmp m v b Void a
initial = const absurd

-- Switch
switch :: forall m v b s s' u u'. Semigroup v => Cmp m v b s u -> Cmp m v b s' u' -> Cmp m v b (Tuple s s') (Either u u')
switch c c' set (Tuple s s') = c (set <<< Left) s <> c' (set <<< Right) s'

infixr 5 switch as &|

-- Switchable
poly :: forall m v b s u. Monoid v => Cmp m v b s u
poly = mempty

-- Codemux
codemux :: forall m v s1 s2 u1 u2. Applicative m => Monoid v => Cmp m v Unit (Either s1 s2) (Either u1 u2) -> (Cmp m v Unit s1 u1 /\ Cmp m v Unit s2 u2)
codemux cmp = cmp1 /\ cmp2
  where
  cmp1 put s = cmp (either put (const $ P.pure unit)) (Left s)
  cmp2 put s = cmp (either (const $ P.pure unit) put) (Right s)

-- Random stuff
handle :: forall m v b s u. (u -> s -> s) -> Cmp m v b s u -> Cmp'' m v b s
handle f c set s = c (flip f s >>> set) s

handle_ :: forall m v b s u. (s -> s) -> Cmp m v b s u -> Cmp'' m v b s
handle_ f = handle (const f)

when :: forall m v s u. Applicative m => (u -> Boolean) -> Cmp m v Unit s u -> Cmp m v Unit s u
when f c set = c set'
  where
  set' u = if f u then set u else P.pure unit

echo :: forall m b s u. Cmp m s b s u
echo _ s = s

-- Given a component that can display a possible value, create a component that can
-- display a list of values
withered :: forall m v b x. Monoid v => Cmp'' m v b (Maybe x) -> Cmp'' m v b (Array x)
withered cmp u s = foldMapWithIndex (\k mt -> cmp (go k) mt) (Just P.<$> s)
  where
  go k Nothing  = u $ compact $ mapWithIndex (\i x -> if k == i then Nothing else Just x) s
  go k (Just t) = u $ mapWithIndex (\i x -> if k == i then t else x) s
