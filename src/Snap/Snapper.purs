module Snap.Snapper where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Apply (lift2)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Data.Either (Either(..), choose, either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Bimodule (class Bimodule, class LeftModule, class RightModule)
import Data.Profunctor.Monoidal (class Monoidal, class Semigroupal, class Unital, poly, (&|))
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))

newtype Snapper m b u s = Snapper { put :: u -> m b, get :: m s }

-- TODO Naming
type Snapper' m s = Snapper m Unit s s
type Snapper'' m b s = Snapper m b s s

runSnapper :: forall m b u s. Snapper m b u s -> { put :: u -> m b, get :: m s }
runSnapper (Snapper x) = x

instance functorSnapper :: Functor m => Functor (Snapper m b u)
  where
  map f (Snapper { put, get }) = Snapper $ { put, get: map f $ get }

instance applySnapper :: Apply m => Apply (Snapper m b u)
  where
  apply (Snapper { get: g1, put: p1 }) (Snapper { get: g2, put: p2 }) = Snapper { get: g1 <*> g2, put: \u -> p1 u *> p2 u }

instance applicativeSnapper :: (Monoid b, Applicative m) => Applicative (Snapper m b u)
  where
  pure s = Snapper { get: pure s, put: const $ pure mempty }

instance bindSnapper :: Bind m => Bind (Snapper m b u)
  where
  bind (Snapper { get, put }) amb = Snapper $ { get: get >>= (\s -> (runSnapper $ amb s).get), put: put }

instance monadSnapper :: (Monoid b, Monad m) => Monad (Snapper m b u)

instance profunctorSnapper :: Functor m => Profunctor (Snapper m b)
  where
  dimap f g (Snapper { put, get }) = Snapper $ { get: map g get, put: put <<< f }

instance ttSemigroupalSnapper :: (Semigroup b, Apply m) => Semigroupal (->) Tuple Tuple Tuple (Snapper m b)
  where
  pzip (Snapper { get: g1, put: p1 } /\ Snapper { get: g2, put: p2 }) = Snapper { get: lift2 (/\) g1 g2, put: \(x /\ y) -> lift2 (<>) (p1 x) (p2 y) }

instance ttUnitalSnapper :: (Monoid b, Applicative m) => Unital (->) Unit Unit Unit (Snapper m b)
  where
  punit _ = Snapper { get: pure unit , put: const $ pure mempty }

instance ttMonoidalSnapepr :: (Monoid b, Applicative m) => Monoidal (->) Tuple Unit Tuple Unit Tuple Unit (Snapper m b)

instance etSemigroupalSnapper :: Apply m => Semigroupal (->) Either Tuple Tuple (Snapper m b)
  where
  pzip (Snapper { get: g1, put: p1 } /\ Snapper { get: g2, put: p2 }) = Snapper { get: lift2 (/\) g1 g2, put: either p1 p2 }

instance etUnitalSnapper :: Applicative m => Unital (->) Void Unit Unit (Snapper m b)
  where
  punit _ = Snapper { get: pure unit, put: absurd }

instance etMonoidalSnapper :: Applicative m => Monoidal (->) Either Void Tuple Unit Tuple Unit (Snapper m b)

instance leftModuleSnapper :: Functor m => LeftModule (->) Tuple Either (Snapper m b)
  where
  lstrength (Snapper { get, put }) = Snapper { get: Left <$> get, put: put <<< fst }

instance eeSemigroupalSnapper :: Alt m => Semigroupal (->) Either Either Tuple (Snapper m b)
  where
  pzip (Snapper { get: g1, put: p1 } /\ Snapper { get: g2, put: p2 }) = Snapper { get: choose g1 g2, put: either p1 p2 }

instance eeUnitalSnapper :: Plus m => Unital (->) Void Void Unit (Snapper m b)
  where
  punit _ = Snapper { get: empty, put: absurd }

instance eeMonoidalSnapper :: Plus m => Monoidal (->) Either Void Either Void Tuple Unit (Snapper m b)

instance rightModuleSnapper :: Functor m => RightModule (->) Tuple Either (Snapper m b)
  where
  rstrength (Snapper { get, put }) = Snapper { get: Right <$> get, put: put <<< snd }

instance teSemigroupalSnapper :: (Semigroup b, Alt m, Apply m) => Semigroupal (->) Tuple Either Tuple (Snapper m b)
  where
  pzip (Snapper { get: g1, put: p1 } /\ Snapper { get: g2, put: p2 }) = Snapper { get: choose g1 g2, put: \(u1 /\ u2) -> lift2 (<>) (p1 u1) (p2 u2) }

instance teUnitalSnapper :: (Monoid b, Plus m, Applicative m) => Unital (->) Unit Void Unit (Snapper m b)
  where
  punit _ = Snapper { get: empty, put: const $ pure mempty }

instance teMonoidalSnapper :: (Monoid b, Plus m, Applicative m) => Monoidal (->) Tuple Unit Either Void Tuple Unit (Snapper m b)

instance semigroupSnapper :: (Semigroup b, Alt m, Apply m) => Semigroup (Snapper m b u s)
  where
  append s1 s2 = dimap (\s -> s /\ s) (either identity identity) $ s1 &| s2

instance monoidSnapper :: (Monoid b, Plus m, Applicative m) => Monoid (Snapper m b u s)
  where
  mempty = poly

instance bimoduleSnapper :: Functor m => Bimodule (->) Tuple Either (Snapper m b)

hoist :: forall m n b u s. (m ~> n) -> Snapper m b u s -> Snapper n b u s
hoist n (Snapper { get, put }) = Snapper { get: n get, put: n <<< put }

instance altSnapper :: Alt m => Alt (Snapper m b u)
  where
  alt (Snapper { get: g1, put: p1 }) (Snapper { get: g2, put: p2 }) = Snapper { get: g1 <|> g2, put: \u -> p1 u <|> p2 u }

instance plusSnapper :: (Monoid b, Plus m) => Plus (Snapper m b u)
  where
  empty = Snapper { get: empty, put: const (empty $> mempty) }

instance alternativeSnapper :: (Monoid b, Alternative m) => Alternative (Snapper m b u)

instance monadZeroSnapper :: (Monoid b, MonadZero m) => MonadZero (Snapper m b u)

instance monadPlusSnapper :: (Monoid b, MonadPlus m) => MonadPlus (Snapper m b u)

reduced :: forall m b u s. Bind m => (u -> s -> m s) -> Snapper'' m b s -> Snapper m b u s
reduced red (Snapper { get, put }) = Snapper { get: get, put: put' }
  where
  put' u = do
    s <- get
    s' <- red u s
    put s'

absorbError :: forall m b u s. Functor m => Snapper m b u (Maybe s) -> Snapper (MaybeT m) b u s
absorbError (Snapper { get, put }) = Snapper { get: MaybeT get, put: MaybeT <<< map Just <<< put }

-- TODO There are probably other ways to write this and similar combinators now that we have the b param
withDefaultState :: forall m b u s. Functor m => s -> Snapper (MaybeT m) b u s -> Snapper m Unit u s
withDefaultState s (Snapper { get, put }) = Snapper { get: map (fromMaybe s) $ runMaybeT $ get, put: void <<< runMaybeT <<< put }
