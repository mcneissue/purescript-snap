module Snap.Component.SYTC where

import Control.Applicative (class Applicative)
import Control.Apply (lift2) as A
import Control.Category (class Category, class Semigroupoid, (<<<), (>>>))
import Data.Either (Either(..), either)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple (Tuple(..), curry, fst, snd, swap, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (Unit, unit, class Semigroup, class Monoid, (<>), ($), (>>=), flip, const, Void, absurd)
import Prelude as P

type Cmp m v s u
  = (u -> m Unit) -> s -> v

type Cmp' m v s
  = Cmp m v s s

-- Lazy2
defer :: forall m v s u. (Unit -> Cmp m v s u) -> Cmp m v s u
defer f set s = f unit set s

-- Semigroup
append :: forall m v s u. Semigroup v => Cmp m v s u -> Cmp m v s u -> Cmp m v s u
append = (<>)

-- Monoid
mempty :: forall m v s u. Monoid v => Cmp m v s u
mempty = P.mempty

-- Profunctor
dimap :: forall m v a' a b b'. (a' -> a) -> (b -> b') -> Cmp m v a b -> Cmp m v a' b'
dimap f g cmp u s = cmp (u <<< g) (f s)

lcmap :: forall m v s s' u. (s -> s') -> Cmp m v s' u -> Cmp m v s u
lcmap f = dimap f P.identity

rmap :: forall m v s u u'. (u -> u') -> Cmp m v s u -> Cmp m v s u'
rmap = dimap P.identity

-- Strong
first :: forall m v s u c. Cmp m v s u -> Cmp m v (Tuple s c) (Tuple u c)
first cmp u s = let c = snd s in cmp (u <<< flip Tuple c) (fst s)

second :: forall m v s u c. Cmp m v s u -> Cmp m v (Tuple c s) (Tuple c u)
second = dimap swap swap <<< first

-- Choice
left :: forall m v s u c. Monoid v => Cmp m v s u -> Cmp m v (Either s c) (Either u c)
left cmp set = either (cmp $ set <<< Left) (const P.mempty)

right :: forall m v s u c. Monoid v => Cmp m v s u -> Cmp m v (Either c s) (Either c u)
right = dimap flipEither flipEither <<< left
  where
  flipEither :: forall a b. Either a b -> Either b a
  flipEither (Left x) = Right x
  flipEither (Right x) = Left x

-- Contrahoistable
contraHoist :: forall v s u m n. (n Unit -> m Unit) -> Cmp m v s u -> Cmp n v s u
contraHoist f cmp set s = cmp (f <<< set) s

-- Functor
map :: forall m u s a b. (a -> b) -> Cmp m a s u -> Cmp m b s u
map = compose2
  where
  compose2 = (<<<) <<< (<<<)

infixl 4 map as <$>!

mapFlipped :: forall m u s a b. Cmp m a s u -> (a -> b) -> Cmp m b s u
mapFlipped = flip map

infixr 4 mapFlipped as <#>!

-- Apply
apply :: forall m u s a b. Cmp m (a -> b) s u -> Cmp m a s u -> Cmp m b s u
apply = lift2 ($)

infixl 4 apply as <*>!

lift2 :: forall m s u a b c. (a -> b -> c) -> Cmp m a s u -> Cmp m b s u -> Cmp m c s u
lift2 f c1 c2 = un Compose $ A.lift2 f (Compose c1) (Compose c2)

-- Applicative
pure :: forall m v s u. v -> Cmp m v s u
pure v _ _ = v

-- Bind
-- NB: All this currying/uncurrying nonsense is going on because I can't figure out
--     a way to express (without newtype overhead) that fn2.bind = (readert fn).bind
bind :: forall m s u a b. Cmp m a s u -> (a -> Cmp m b s u) -> Cmp m b s u
bind c f = curry $ (uncurry c) >>= (f >>> uncurry)

infixl 1 bind as >>=!

-- Semigroupoid
compose :: forall m s u c x y z. Semigroupoid c => Cmp m (c y z) s u -> Cmp m (c x y) s u -> Cmp m (c x z) s u
compose = lift2 (<<<)

infixl 9 compose as <<<!

composeFlipped :: forall m s u c x y z. Semigroupoid c => Cmp m (c x y) s u -> Cmp m (c y z) s u -> Cmp m (c x z) s u
composeFlipped = flip compose

infixr 9 composeFlipped as >>>!

-- Category
identity :: forall m s u c x. Category c => Cmp m (c x x) s u
identity = pure P.identity

-- Contrafilterable
lcmapMaybe :: forall m v s s' u. Monoid v => (s -> Maybe s') -> Cmp m v s' u -> Cmp m v s u
lcmapMaybe p cmp put s = case p s of
  Nothing -> P.mempty
  Just s' -> cmp put s'

-- Demux
demux :: forall m v a b c d. Cmp m v a b -> Cmp m v c d -> Cmp m v (Either a c) (Either b d)
demux f g set = either (f $ set <<< Left) (g $ set <<< Right)

-- Demuxative
stop :: forall m v a. Cmp m v Void a
stop = const absurd

-- Splice
splice :: forall m v s s' u u'. Semigroup v => Cmp m v s u -> Cmp m v s' u' -> Cmp m v (Tuple s s') (Either u u')
splice c c' set (Tuple s s') = c (set <<< Left) s <> c' (set <<< Right) s'

-- Spliceable
default :: forall m v s u. Monoid v => Cmp m v s u
default = mempty

-- Codemux
codemux :: forall m v s1 s2 u1 u2. Applicative m => Monoid v => Cmp m v (Either s1 s2) (Either u1 u2) -> (Cmp m v s1 u1 /\ Cmp m v s2 u2)
codemux cmp = cmp1 /\ cmp2
  where
  cmp1 put s = cmp (either put (const $ P.pure unit)) (Left s)
  cmp2 put s = cmp (either (const $ P.pure unit) put) (Right s)

-- Random stuff
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
