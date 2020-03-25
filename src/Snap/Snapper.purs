module Snap.Snapper where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Apply (lift2)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Data.Either (Either(..), choose, either)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Bimodule (class Bimodule, class LeftModule, class RightModule)
import Data.Profunctor.Monoidal (class Monoidal, class Semigroupal, class Unital)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))

newtype Snapper m u s = Snapper { put :: u -> m Unit, get :: m s }

type Snapper' m s = Snapper m s s

runSnapper :: forall m s u. Snapper m u s -> { put :: u -> m Unit, get :: m s }
runSnapper (Snapper x) = x

instance functorSnapper :: Functor m => Functor (Snapper m u)
  where
  map f (Snapper { put, get }) = Snapper $ { put, get: map f $ get }

instance applySnapper :: Apply m => Apply (Snapper m u)
  where
  apply (Snapper { get: g1, put: p1 }) (Snapper { get: g2, put: p2 }) = Snapper { get: g1 <*> g2, put: \u -> p1 u *> p2 u }

instance applicativeSnapper :: Applicative m => Applicative (Snapper m u)
  where
  pure s = Snapper { get: pure s, put: const $ pure unit }

instance bindSnapper :: Bind m => Bind (Snapper m u)
  where
  bind (Snapper { get, put }) amb = Snapper $ { get: get >>= (\s -> (runSnapper $ amb s).get), put: put }

instance monadSnapper :: Monad m => Monad (Snapper m u)

instance profunctorSnapper :: Functor m => Profunctor (Snapper m)
  where
  dimap f g (Snapper { put, get }) = Snapper $ { get: map g get, put: put <<< f }

instance ttSemigroupalSnapper :: Apply m => Semigroupal (->) Tuple Tuple Tuple (Snapper m)
  where
  pzip (Snapper { get: g1, put: p1 } /\ Snapper { get: g2, put: p2 }) = Snapper { get: lift2 (/\) g1 g2, put: \(x /\ y) -> lift2 (<>) (p1 x) (p2 y) }

instance ttUnitalSnapper :: Applicative m => Unital (->) Unit Unit Unit (Snapper m)
  where
  punit _ = Snapper { get: pure unit, put: pure }

instance ttMonoidalSnapepr :: Applicative m => Monoidal (->) Tuple Unit Tuple Unit Tuple Unit (Snapper m)

instance etSemigroupalSnapper :: Apply m => Semigroupal (->) Either Tuple Tuple (Snapper m)
  where
  pzip (Snapper { get: g1, put: p1 } /\ Snapper { get: g2, put: p2 }) = Snapper { get: lift2 (/\) g1 g2, put: either p1 p2 }

instance etUnitalSnapper :: Applicative m => Unital (->) Void Unit Unit (Snapper m)
  where
  punit _ = Snapper { get: pure unit, put: absurd }

instance etMonoidalSnapper :: Applicative m => Monoidal (->) Either Void Tuple Unit Tuple Unit (Snapper m)

instance leftModuleSnapper :: Functor m => LeftModule (->) Tuple Either (Snapper m)
  where
  lstrength (Snapper { get, put }) = Snapper { get: Left <$> get, put: put <<< fst }

instance eeSemigroupalSnapper :: Alt m => Semigroupal (->) Either Either Tuple (Snapper m)
  where
  pzip (Snapper { get: g1, put: p1 } /\ Snapper { get: g2, put: p2 }) = Snapper { get: choose g1 g2, put: either p1 p2 }

instance eeUnitalSnapper :: Plus m => Unital (->) Void Void Unit (Snapper m)
  where
  punit _ = Snapper { get: empty, put: absurd }

instance eeMonoidalSnapper :: Plus m => Monoidal (->) Either Void Either Void Tuple Unit (Snapper m)

instance rightModuleSnapper :: Functor m => RightModule (->) Tuple Either (Snapper m)
  where
  rstrength (Snapper { get, put }) = Snapper { get: Right <$> get, put: put <<< snd }

instance bimoduleSnapper :: Functor m => Bimodule (->) Tuple Either (Snapper m)

hoist :: forall m n s u. (m ~> n) -> Snapper m s u -> Snapper n s u
hoist n (Snapper { get, put }) = Snapper { get: n get, put: n <<< put }

instance altSnapper :: Alt m => Alt (Snapper m u)
  where
  alt (Snapper { get: g1, put: p1 }) (Snapper { get: g2, put: p2 }) = Snapper { get: g1 <|> g2, put: \u -> p1 u <|> p2 u }

instance plusSnapper :: Plus m => Plus (Snapper m u)
  where
  empty = Snapper { get: empty, put: const empty }

instance alternativeSnapper :: Alternative m => Alternative (Snapper m u)

instance monadZeroSnapper :: MonadZero m => MonadZero (Snapper m u)

instance monadPlusSnapper :: MonadPlus m => MonadPlus (Snapper m u)

reduced :: forall m u s. Bind m => (u -> s -> m s) -> Snapper' m s -> Snapper m u s
reduced red (Snapper { get, put }) = Snapper { get: get, put: put' }
  where
  put' u = do
    s <- get
    s' <- red u s
    put s'
