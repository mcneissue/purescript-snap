module Data.Record.Choose (HasField, hasField, getField, choose) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Prim.Row (class Cons)
import Record.Unsafe (unsafeGet)
import Unsafe.Coerce (unsafeCoerce)

newtype HasField (r :: # Type) = HasField String

hasField :: forall s a r' r. IsSymbol s => Cons s a r' r => SProxy s -> HasField r
hasField s = HasField $ reflectSymbol s

getField :: forall r. HasField r -> String
getField (HasField s) = s

choose :: forall r. HasField r -> Record r -> Variant r
choose (HasField k) r = coerceV $ VariantRep { type: k, value: unsafeGet k r }
  where
  coerceV :: forall a. VariantRep a -> Variant r
  coerceV = unsafeCoerce
