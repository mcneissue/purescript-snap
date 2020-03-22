module Snap.Component.SYTC where

import Control.Applicative (class Applicative)
import Control.Apply (lift2) as A
import Control.Category (class Category, class Semigroupoid, (<<<), (>>>))
import Data.Either (Either(..), either)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry)
import Prelude (Unit, unit, class Semigroup, class Monoid, (<>), ($), (>>=), flip, const, Void, absurd)
import Prelude as P

type Cmp m v s u
  = (u -> m Unit) -> s -> v

type Cmp' m v s
  = Cmp m v s s

defer :: forall m v s u. (Unit -> Cmp m v s u) -> Cmp m v s u
defer f set s = f unit set s

append :: forall m v s u. Semigroup v => Cmp m v s u -> Cmp m v s u -> Cmp m v s u
append = (<>)

mempty :: forall m v s u. Monoid v => Cmp m v s u
mempty = P.mempty

dimap :: forall m v a' a b b'. (a' -> a) -> (b -> b') -> Cmp m v a b -> Cmp m v a' b'
dimap f g cmp u s = cmp (u <<< g) (f s)

lcmap :: forall m v s s' u. (s -> s') -> Cmp m v s' u -> Cmp m v s u
lcmap f = dimap f P.identity

rmap :: forall m v s u u'. (u -> u') -> Cmp m v s u -> Cmp m v s u'
rmap = dimap P.identity

first :: forall m v s u c. Cmp m v s u -> Cmp m v (Tuple s c) (Tuple u c)
first cmp u s = let c = snd s in cmp (u <<< flip Tuple c) (fst s)

second :: forall m v s u c. Cmp m v s u -> Cmp m v (Tuple c s) (Tuple c u)
second = dimap swap swap <<< first

left :: forall m v s u c. Monoid v => Cmp m v s u -> Cmp m v (Either s c) (Either u c)
left cmp set = either (cmp $ set <<< Left) (const P.mempty)

right :: forall m v s u c. Monoid v => Cmp m v s u -> Cmp m v (Either c s) (Either c u)
right = dimap flipEither flipEither <<< left
  where
  flipEither :: forall a b. Either a b -> Either b a
  flipEither (Left x) = Right x
  flipEither (Right x) = Left x

switch :: forall m v a b c d. Cmp m v a b -> Cmp m v c d -> Cmp m v (Either a c) (Either b d)
switch f g set = either (f $ set <<< Left) (g $ set <<< Right)

never :: forall m v. Cmp' m v Void
never = const absurd

zip :: forall m v s s' u u'. Semigroup v => Cmp m v s u -> Cmp m v s' u' -> Cmp m v (Tuple s s') (Either u u')
zip c c' set (Tuple s s') = c (set <<< Left) s <> c' (set <<< Right) s'

infinite :: forall m v s u. Monoid v => Cmp m v s u
infinite = mempty

toEndo :: forall a. a -> Endo Function a
toEndo = Endo <<< const

runEndo :: forall a. Endo Function a -> (a -> a)
runEndo (Endo f) = f

contraHoist :: forall v s u m n. (n Unit -> m Unit) -> Cmp m v s u -> Cmp n v s u
contraHoist f cmp set s = cmp (f <<< set) s

pure :: forall m v s u. v -> Cmp m v s u
pure v _ _ = v

map :: forall m u s a b. (a -> b) -> Cmp m a s u -> Cmp m b s u
map = compose2
  where
  compose2 = (<<<) <<< (<<<)

infixl 4 map as <$>!

mapFlipped :: forall m u s a b. Cmp m a s u -> (a -> b) -> Cmp m b s u
mapFlipped = flip map

infixr 4 mapFlipped as <#>!

apply :: forall m u s a b. Cmp m (a -> b) s u -> Cmp m a s u -> Cmp m b s u
apply = lift2 ($)

infixl 4 apply as <*>!

lift2 :: forall m s u a b c. (a -> b -> c) -> Cmp m a s u -> Cmp m b s u -> Cmp m c s u
lift2 f c1 c2 = un Compose $ A.lift2 f (Compose c1) (Compose c2)

-- All this currying/uncurrying nonsense is going on because I can't figure out
-- a way to express (without newtype overhead) that fn2.bind = (readert fn).bind
bind :: forall m s u a b. Cmp m a s u -> (a -> Cmp m b s u) -> Cmp m b s u
bind c f = curry $ (uncurry c) >>= (f >>> uncurry)

infixl 1 bind as >>=!

identity :: forall m s u c x. Category c => Cmp m (c x x) s u
identity = pure P.identity

compose :: forall m s u c x y z. Semigroupoid c => Cmp m (c y z) s u -> Cmp m (c x y) s u -> Cmp m (c x z) s u
compose = lift2 (<<<)

infixl 9 compose as <<<!

composeFlipped :: forall m s u c x y z. Semigroupoid c => Cmp m (c x y) s u -> Cmp m (c y z) s u -> Cmp m (c x z) s u
composeFlipped = flip compose

infixr 9 composeFlipped as >>>!

handle :: forall m v s u. (u -> s -> s) -> Cmp m v s u -> Cmp' m v s
handle f c set s = c (flip f s >>> set) s

handle_ :: forall m v s u. (s -> s) -> Cmp m v s u -> Cmp' m v s
handle_ f = handle (const f)

when :: forall m v s u. Applicative m => (u -> Boolean) -> Cmp m v s u -> Cmp m v s u
when f c set = c set'
  where
  set' u = if f u then set u else P.pure unit

echo :: forall m s u. Cmp m s s u
echo _ s = s

lcmapMaybe :: forall m v s s' u. Monoid v => (s -> Maybe s') -> Cmp m v s' u -> Cmp m v s u
lcmapMaybe p cmp put s = case p s of
  Nothing -> P.mempty
  Just s' -> cmp put s'
