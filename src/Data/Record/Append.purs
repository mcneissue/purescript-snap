module Data.Record.Append where

import Prelude

import Data.Functor.Variant (SProxy(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Prelude (class RowToList, RLProxy(..))

-- @natefaubion in the #purescript room came up with this much simpler implementation of squishy append for records
-- https://gist.github.com/natefaubion/d2a6f1965fdaa01f0eb49bd753ccaa4a

class AppendRecord r1 r2 r3 | r1 r2 -> r3 where
  append :: r1 -> r2 -> r3

instance appendRecordImpl ::
  ( Row.Union r1 r2 rx
  , RowToList rx rl
  , AppendRowList rl r1 r2 () r3
  , Row.Union r3 rx r4
  , Row.Nub r4 r5
  ) =>
  AppendRecord { | r1 } { | r2 } { | r5 } where
  append a b =
    Record.nub (Record.union (appendRL (RLProxy :: RLProxy rl) {} a b) (Record.union a b))

class AppendRowList (rl :: RowList) r1 r2 ri ro | rl r1 r2 ri -> ro where
  appendRL :: RLProxy rl -> { | ri } -> { | r1 } -> { | r2 } -> { | ro }

instance appendRowList1 ::
  ( AppendRowList rest r1 r2 ri' ro
  , IsSymbol sym
  , Row.Lacks sym ri
  , Row.Cons sym a ri ri'
  , Row.Cons sym a rx1 r1
  , Row.Cons sym a rx2 r2
  , Semigroup a
  ) =>
  AppendRowList (RowList.Cons sym a (RowList.Cons sym a rest)) r1 r2 ri ro where
  appendRL _ ri a b =
    appendRL
      (RLProxy :: RLProxy rest)
      (Record.insert sym (Record.get sym a <> Record.get sym b) ri :: { | ri' })
      a b
    where
    sym = SProxy :: SProxy sym
else instance appendRowList2 ::
  ( AppendRowList rest r1 r2 ri ro
  ) =>
  AppendRowList (RowList.Cons sym a rest) r1 r2 ri ro where
  appendRL _ ri a b =
    appendRL (RLProxy :: RLProxy rest) ri a b
else instance appendRowList3 ::
  AppendRowList RowList.Nil r1 r2 ri ri where
    appendRL _ ri _ _ = ri
