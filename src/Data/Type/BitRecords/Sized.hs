-- | Size Fields
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Sized
  ( type Sized, type Sized8, type Sized16, Sized32, type Sized64
  , type SizedField, type SizedField8, type SizedField16, type SizedField32, type SizedField64)
  where

import Data.Type.BitRecords.Core
import GHC.TypeLits
import Data.Kind.Extra
import Data.Kind (type Type)

-- | A record with a /size/ member, and a nested record that can be counted
-- using 'SizeInBytes'.
data Sized
  (sf :: Extends (BitRecordField (t :: BitField (rt :: Type) Nat (size :: Nat))))
  (r :: BitRecord)
  :: Extends BitRecord
type instance From (Sized sf r) =
   "size" @: sf := SizeInBytes r .+: r

-- | A convenient alias for a 'Sized' with an 'FieldU8' size field.
type Sized8 t = Sized FieldU8 t

-- | A convenient alias for a 'Sized' with an 'FieldU16' size field.
type Sized16 t = Sized FieldU16 t

-- | A convenient alias for a 'Sized' with an 'FieldU32' size field.
type Sized32 t = Sized2 FieldU32 t

-- | A convenient alias for a 'Sized' with an 'FieldU32' size field.
data Sized2 ::Extends (BitField rt Nat sz) -> Extends a -> Extends BitRecord

type instance From (Sized2 sizeField (r :: Extends BitRecord)) =
  'RecordField ("size" @:: sizeField :=. SizeInBytes (From r)) :+: From r

type instance From (Sized2 sizeField (r :: Extends (BitField sr st sz))) =
  'RecordField ("size" @:: sizeField :=. SizeInBytes (From r)) :+: 'RecordField r

-- | A convenient alias for a 'Sized' with an 'FieldU64' size field.
type Sized64 t = Sized FieldU64 t

-- | A record with a /size/ member, and a nested field that can be counted
-- using 'SizeInBytes'.
data SizedField
  (sf :: Extends (BitRecordField (t :: BitField (rt :: Type) Nat (size :: Nat))))
  (r :: Extends (BitRecordField (u :: BitField (rt' :: Type) (st' :: k0) (len0 :: Nat))))
  :: Extends BitRecord
type instance From (SizedField sf r) =
   "size" @: sf := SizeInBytes r .+. r

-- | A convenient alias for a 'SizedField' with an 'FieldU8' size field.
type SizedField8 t = SizedField FieldU8 t

-- | A convenient alias for a 'SizedField' with an 'FieldU16' size field.
type SizedField16 t = SizedField FieldU16 t

-- | A convenient alias for a 'SizedField' with an 'FieldU32' size field.
type SizedField32 t = Sized32 (Konst ('RecordField t))

-- | A convenient alias for a 'SizedField' with an 'FieldU64' size field.
type SizedField64 t = SizedField FieldU64 t
