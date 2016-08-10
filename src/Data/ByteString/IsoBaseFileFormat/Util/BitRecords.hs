{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.IsoBaseFileFormat.Util.BitRecords where

import Data.Kind
import Data.Word
import Data.Type.Bool
import GHC.TypeLits
import Data.Bits
import Data.Proxy
import Test.TypeSpecCrazy

-- * Fields

-- | Define a field with a size
data Field :: Nat -> Type

-- | Alias for a single bit field
type Flag = Field 1

-- | Get the field size of a field
type family GetFieldSize field :: Nat
type instance GetFieldSize (Field n) = n

type FieldPosition = (Nat, Nat)

-- * Records

-- | Combine two fields to form a new field.
--
-- This field composition happens in the order from TODO wrong DOC
-- the *most significant bit* to the *least significant bit*
--
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: \------f0-----/\--------f1--------/\--- empty ---/
-- @
--
data (:>:) :: Type -> Type -> Type
infixl 3 :>:

-- * Nested Records

-- | A field with a name
data (:=>) :: label -> Type -> Type where
infixr 5 :=>

-- | A field with a constant fixed value
data (:=) :: Type -> Nat -> Type
infixr 6 :=

-- | Make a part of the record ignored
data Ignore :: Type -> Type

-- | A Path of field labels for nested record created with ':=>'
data (:/) :: Symbol -> label -> Type
infixr 7 :/

-- | A wrapper around 'Constraint' that propagates 'TypeError'.
type ConstraintE = Either Constraint Constraint

-- | Unwrap a 'ConstraintE', this is where 'TypeError's might be /thrown/.
type family
  RunConstraintE t :: Constraint where
  RunConstraintE ('Left t) = t
  RunConstraintE ('Right t) = t

-- * BitRecord Accessor

type family
  GetRecordSize (r :: rk) :: Nat where
  GetRecordSize (label :=> f) = GetRecordSize f
  GetRecordSize (l :>: r)     = GetRecordSize l + GetRecordSize r
  GetRecordSize (Ignore r)    = GetRecordSize r
  GetRecordSize (Field n)     = n
  GetRecordSize (r := v)      = GetRecordSize r

type family
  HasField (r :: rk) (l :: lk) :: Bool where
  HasField (l :=> f) l        = 'True
  HasField (l :=> f) (l :/ p) = HasField f p
  HasField (f1 :>: f2) l      = HasField f1 l || HasField f2 l
  HasField (r := v) l         = HasField r l
  HasField (Ignore r) l       = HasField r l
  HasField f l                = 'False

type family
  HasFieldConstraint (r :: rk) (l :: lk) :: ConstraintE where
  HasFieldConstraint r l =
      If (HasField r l)
         ('Left (HasField r l ~ 'True))
         ('Right
           (TypeError ('Text "Label not found: '"
                       ':<>: 'ShowType l
                       ':<>: 'Text "' in:"
                       ':$$: 'ShowType r )))

-- field location and access

type family
  GetFieldPosition (r :: rk) (l :: lk) :: Result FieldPosition where
  GetFieldPosition f l =
     If (HasField f l)
       ('Right (GetFieldPositionUnsafe f l))
       ('Left ('Text "Label not found. Cannot get bit range for '"
          ':<>: 'ShowType l
          ':<>: 'Text "' in:"
          ':$$: 'ShowType f ))

type family
  GetFieldPositionUnsafe (r :: rk) (l :: lk) :: FieldPosition where
  GetFieldPositionUnsafe (l :=> f)  l        = '(0, GetRecordSize f - 1)
  GetFieldPositionUnsafe (l :=> f)  (l :/ p) = GetFieldPositionUnsafe f p
  GetFieldPositionUnsafe (f := v)   l        = GetFieldPositionUnsafe f l
  GetFieldPositionUnsafe (Ignore f) l        = GetFieldPositionUnsafe f l
  GetFieldPositionUnsafe (f :>: f') l        =
     If (HasField f l)
      (GetFieldPositionUnsafe f l)
      (AddToFieldPosition (GetRecordSize f) (GetFieldPositionUnsafe f' l))

type family
  AddToFieldPosition (v :: Nat) (e :: (Nat, Nat)) :: (Nat, Nat) where
  AddToFieldPosition v '(a,b) = '(a + v, b + v)

type family
  IsFieldPostition (pos :: FieldPosition) :: Constraint where
  IsFieldPostition '(a, b) =
    If (a <=? b)
       (a <= b, KnownNat a, KnownNat b)
       (TypeError
         ('Text "Bad field position: " ':<>: 'ShowType '(a,b)
          ':$$: 'Text "First index greater than last: "
          ':<>: 'ShowType a
          ':<>: 'Text " > "
          ':<>: 'ShowType b ))

type family
  FieldPostitionToList (pos :: FieldPosition) :: [Nat] where
    FieldPostitionToList '(a, a) = '[a]
    FieldPostitionToList '(a, b) = (a ': (FieldPostitionToList '(a+1, b)))

type Align padRight a f =
    AddPadding padRight ((a - (GetRecordSize f `Rem` a)) `Rem` a) f

type family
  AddPadding (padRight :: Bool) (n :: Nat) (r :: rk) :: rk where
  AddPadding padRight 0 r = r
  AddPadding 'True n r = r :>: Ignore (Field n := 0)
  AddPadding 'False n r = Ignore (Field n := 0) :>: r

-- | Get the remainder of the integer division of x and y, such that @forall x
-- y. exists k. (Rem x y) == x - y * k@ The algorithm is: count down x
-- until zero, incrementing the accumulator at each step. Whenever the
-- accumulator is equal to y set it to zero.
--
-- If the accumulator has reached y reset it. It is important to do this
-- BEFORE checking if x == y and then returning the accumulator, for the case
-- where x = k * y with k > 0. For example:
--
-- @
--  6 `Rem` 3     = RemImpl 6 3 0
--  RemImpl 6 3 0 = RemImpl (6-1) 3 (0+1)   -- RemImpl Clause 4
--  RemImpl 5 3 1 = RemImpl (5-1) 3 (1+1)   -- RemImpl Clause 4
--  RemImpl 4 3 2 = RemImpl (4-1) 3 (2+1)   -- RemImpl Clause 4
--  RemImpl 3 3 3 = RemImpl 3 3 0           -- RemImpl Clause 2 !!!
--  RemImpl 3 3 0 = 0                       -- RemImpl Clause 3 !!!
-- @
--
type Rem (x :: Nat) (y :: Nat) = RemImpl x y 0
type family
  RemImpl (x :: Nat) (y :: nat) (acc :: Nat) :: Nat where
  -- finished if x was < y:
  RemImpl 0 y acc = acc
  RemImpl x y y   = RemImpl x y 0
  -- finished if x was >= y:
  RemImpl y y acc = acc
  -- the base case
  RemImpl x y acc = RemImpl (x - 1) y (acc + 1)

-- | Integer division of x and y: @Div x y  ==> x / y@,
-- NOTE This only works for small numbers currently
type Div (x :: Nat) (y :: Nat) = DivImpl (x - (x `Rem` y)) y 0
type family
  DivImpl (x :: Nat) (y :: nat) (acc :: Nat) :: Nat where
  DivImpl 0 y acc = acc
  DivImpl x y acc = If (x + 1 <=? y) acc (DivImpl (x - y) y (acc + 1))

-- * Bit manipulation

type family TestHighBit (x :: Nat) (n :: Nat) :: Bool where
  TestHighBit x n = ((2 ^ n) <=? x) -- x > 2^n

type ToBits x n = ToBits_ x n 'False
type family ToBits_ (x :: Nat) (n :: Nat) (started :: Bool) :: [Bool] where
  ToBits_ x 0 started = '[]
  ToBits_ x n started = ToBitsInner (TestHighBit x (n - 1)) x (n - 1) started
type family
  ToBitsInner (highBitSet :: Bool) (x :: Nat) (n :: Nat) (started :: Bool) :: [Bool] where
  ToBitsInner 'True  x n started = 'True  ': ToBits_ (x - 2^n) n 'True
  ToBitsInner 'False x n 'False  =           ToBits_ x         n 'False
  ToBitsInner 'False x n 'True   = 'False ': ToBits_ x         n 'True

type FromBits bits = FromBits_ bits 0
type family FromBits_ (bits :: [Bool]) (acc :: Nat) :: Nat where
  FromBits_ '[] acc = acc
  FromBits_ ('False ': rest) acc = FromBits_ rest (acc + acc)
  FromBits_ ('True  ': rest) acc = FromBits_ rest (1 + acc + acc)

type family
  ShiftBitsR (bits :: [Bool]) (n :: Nat) :: [Bool] where
  ShiftBitsR bits 0 = bits
  ShiftBitsR '[] n = '[]
  ShiftBitsR '[e] 1 = '[]
  ShiftBitsR (e ': rest) 1 = e ': ShiftBitsR rest 1
  ShiftBitsR (e ': rest) n = ShiftBitsR (ShiftBitsR (e ': rest) 1) (n - 1)

type family
  GetMostSignificantBitIndex (highestBit :: Nat) (n :: Nat) :: Nat where
  GetMostSignificantBitIndex          0 n = 1
  GetMostSignificantBitIndex highestBit n =
    If  (2 ^ (highestBit + 1) <=? n)
        (TypeError ('Text "number to big: "
                    ':<>: 'ShowType n
                    ':<>: 'Text " >= "
                    ':<>: 'ShowType (2 ^ (highestBit + 1))))
        (If (2 ^ highestBit <=? n)
            highestBit
            (GetMostSignificantBitIndex (highestBit - 1) n))

-- | Shift a type level natural to the right. This useful for division by powers
-- of two.
type family
  ShiftR (xMaxBits :: Nat) (x :: Nat) (bits :: Nat) :: Nat where
  ShiftR xMaxBits x n =
    FromBits
      (ShiftBitsR
        (ToBits x
                (1 + (GetMostSignificantBitIndex xMaxBits x)))
        n)

-- * Bit record accessor for 'Num's

-- | Return the value of a single bit field as Bool
getFlag
  :: forall a (field :: fk) (first :: Nat) record p1 p2
  . ( IsFieldC field record first first
    , Bits a )
   => p1 field -> p2 record -> a -> Bool
getFlag _ _ a = testBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)

setFlag
  :: forall a field (first :: Nat) record p1 p2
  . ( IsFieldC field record first first
    , Bits a )
   => p1 field -> p2 record -> Bool -> a -> a
setFlag _ _ v a = modifyBit a pos
    where pos = fromIntegral $ natVal (Proxy :: Proxy first)
          modifyBit = if v then setBit else clearBit

getField
  :: forall a b field (first :: Nat) (last :: Nat) record pxy1 pxy2
  . ( IsFieldC field record first last
    , Integral a
    , Bits a
    , Num b)
   => pxy1 field -> pxy2 record -> a -> b
getField _ _ a = fromIntegral ((a `shiftR` posFirst) .&. bitMask)
    where
      bitMask =
        let bitCount = 1 + posLast - posFirst
            in (2 ^ bitCount) - 1
      posFirst = fromIntegral $ natVal (Proxy :: Proxy first)
      posLast = fromIntegral $ natVal (Proxy :: Proxy last)

setField
  :: forall a b field (first :: Nat) (last :: Nat) record pxy1 pxy2
  . ( IsFieldC field record first last
    , Num a
    , Bits a
    , Integral b)
   => pxy1 field -> pxy2 record -> b -> a -> a
setField _ _ v x = (x .&. bitMaskField) .|. (v' `shiftL` posFirst)
    where
      v' = bitMaskValue .&. fromIntegral v
      bitMaskField = complement (bitMaskValue `shiftL` posFirst)
      bitMaskValue =
        let bitCount = 1 + posLast - posFirst
            in (2 ^ bitCount) - 1
      posFirst = fromIntegral $ natVal (Proxy :: Proxy first)
      posLast = fromIntegral $ natVal (Proxy :: Proxy last)

type IsFieldC field record first last =
    ( RunConstraintE (record `HasFieldConstraint` field)
     , KnownNat first
     , KnownNat last
     , 'Right '(first, last) ~ (GetFieldPosition record field)
     )