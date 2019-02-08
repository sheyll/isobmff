{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Structure.TypeLits
  ( PositiveInt
  , NegativeInt
  , Bits
  , LiteralFamily(..)
  , To
  , Value
  , Empty
  , Bit
  , Byte
  , Signed(..)
  , SignedByte
  , SignedInteger
  , Sequence
  )
where

import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Pretty
import           Data.Kind
import           Data.Word
import           GHC.TypeLits


-- * Type Level Literals

-- | A 'Positive' 'Signed' 'Nat'
type family PositiveInt (n :: Nat) where
  PositiveInt n = To (Signed Nat Nat) ('Positive n :: Signed Nat Nat)

-- | A 'Negative' 'Signed' 'Nat'
type family NegativeInt (n :: Nat) where
  NegativeInt n = To (Signed Nat Nat) ('Negative n :: Signed Nat Nat)

-- | A literal bit sequence. The sequence is validated, only
-- zeros and ones are allowed.
type family Bits (xs :: [Nat]) where
    Bits xs = To (Sequence Bit Nat) xs

-- * Type Family Supporting Type Level Literals

--
-- | A /kind/ class for /kinds/, that are /inhabited/ on  __type-level__ by a certain kind of literal.
--
-- [@a@] This parameter identifies the whole family; a type level literal kind of the family is determined by
-- that parameter. No two instances for the same family but with different type level literal kinds are
-- allowed.
--
-- [@typeLitKind@] This is the kind of __type level literals__ /representing/ the static values inhabiting
-- this family.
--
class LiteralFamily a typeLitKind | a -> typeLitKind where

  -- | A __predicate__ that is applied to a type-level-literal and is satisfied /iff/ the given literal
  -- is __in the range of valid type-level-literals__ of kind @typeLitKind@.
  --
  -- This is by 'SequenceLiteral' for example.
  type InRange a (typeLit :: typeLitKind) :: Bool
  type InRange a typeLit = 'True

  -- | Convert a type level representation to a 'PrettyType'
  type Pretty a (typeLit :: typeLitKind) :: PrettyType
  type Pretty a (typeLit :: typeLitKind) = ToPretty typeLit
    -- TODO decide to use or ditch this default: "TypeLiteral" <:$$--> ToPretty a <++> PutStr " " <++> ToPretty typeLit <++> PutStr "::" <++> ToPretty typeLitKind

  -- | Seperator to use when rendering a list of literals of this family.
  type PrettySeperator a (typeLit :: typeLitKind) :: PrettyType
  type PrettySeperator a d = 'PrettySpace

  -- | Return the number of bits that the literal value will occupy
  type SizeOf a (typeLit :: typeLitKind) :: Nat

-- | Make a valid 'Value' by checking the parameters using 'InRange' and return a 'Value'
type family To s (x :: k) where
  To s (x :: k) = If (InRange s x) (Value s k x)
    (TypeError ('Text "Cannot make a " ':<>: 'ShowType s ':<>: 'Text " from: " ':<>: 'ShowType x ))

-- | The representation of a /value/ belonging to a 'LiteralFamily'
data Value s k (x :: k) :: Type

type instance ToPretty (Value s k x) = Pretty s x

-- | An empty type literal
data Empty = Empty

type instance ToPretty Empty = PutStr "Empty"
type instance ToPretty 'Empty = PutStr "Empty"

instance LiteralFamily Empty Empty where
  type InRange Empty 'Empty = 'True
  type Pretty Empty 'Empty = 'PrettyEmpty
  type SizeOf Empty 'Empty = 0

instance LiteralFamily () () where
  type InRange () '() = 'True
  type Pretty () '() = 'PrettyEmpty
  type SizeOf () '() = 1

-- | A bit literal
data Bit

type instance ToPretty Bit = PutStr "Bit"

instance LiteralFamily Bit Nat where
  type InRange Bit n = ( n == 0 || n == 1 )
  type Pretty Bit n = PutNat n
  type SizeOf Bit _ = 1
  type PrettySeperator Bit _ = 'PrettyEmpty

-- | An unsigned byte literal
data Byte

type instance ToPretty Byte = PutStr "Byte"

instance LiteralFamily Byte Nat where
  type InRange Byte n = ( n <=? 255 )
  type Pretty Byte n = PutHex n
  type SizeOf Byte _ = 8

instance LiteralFamily Word8 Nat where
  type InRange Word8 n = ( n <=? 255 )
  type Pretty Word8 n = PutNat n
  type SizeOf Word8 _ = 8

instance LiteralFamily Word16 Nat where
  type InRange Word16 n = ( n <=? 65535 )
  type Pretty Word16 n = PutNat n
  type SizeOf Word16 _ = 16

instance LiteralFamily Nat Nat where
  type InRange Nat n = 'True
  type Pretty Nat n = PutNat n
  type SizeOf Nat x = NatSize x

instance LiteralFamily Bool Bool where
  type InRange Bool n = 'True
  type Pretty Bool n = ToPretty n
  type SizeOf Bool x = 1

instance (LiteralFamily as ak, LiteralFamily bs bk) => LiteralFamily (as, bs) (ak, bk) where
  type InRange (as, bs) '(a, b) = InRange as a && InRange bs b
  type Pretty (as, bs) '(a, b) = PrettyParens (PrettyWide '[Pretty as a, Pretty bs b])
  type SizeOf (as, bs) '(a, b) = SizeOf as a + SizeOf bs b

instance (LiteralFamily s k) => LiteralFamily (Maybe s) (Maybe k) where
  type InRange (Maybe s) 'Nothing = 'True
  type InRange (Maybe s) ('Just x) = InRange s x
  type Pretty (Maybe s) 'Nothing = ToPretty s <++> PutStr ":" <+> PutStr "n/a"
  type Pretty (Maybe s) ('Just x) = ToPretty s <++> PutStr ":" <+> Pretty s x
  type SizeOf (Maybe s) 'Nothing = 0
  type SizeOf (Maybe s) ('Just x) = SizeOf s x


-- | The minimum  number of bits to represent the value
type family NatSize (value :: Nat) :: Nat where
  NatSize 0 = 1
  NatSize n = Log2 n + 1

-- | A signed type literal for family @t@ over GHC type literal @v@
data Signed t v = Positive v | Negative v

type instance ToPretty (Signed t v) = "Signed" <:> ToPretty t <+> ToPretty v
type instance ToPretty ('Positive v) = PutStr "+" <++> ToPretty v
type instance ToPretty ('Negative v) = PutStr "-" <++> ToPretty v


instance LiteralFamily t v => LiteralFamily (Signed t v) (Signed t v) where
  type instance InRange (Signed t v) ( 'Positive x) = InRange t x
  type instance InRange (Signed t v) ( 'Negative x) = InRange t x
  type instance Pretty (Signed t v) ( 'Positive x) = PutStr "+" <+> Pretty t x
  type instance Pretty (Signed t v) ( 'Negative x) = PutStr "-" <+> Pretty t x
  type instance SizeOf (Signed t v) ( 'Positive x) = SizeOf t x + 1
  type instance SizeOf (Signed t v) ( 'Negative x) = SizeOf t x + 1

-- | A signed 'Byte'
type SignedByte = Signed Byte Nat

-- | A signed 'Nat'
type SignedInteger = Signed Nat Nat

-- ** Composite Literals

-- | A sequence of literals.
data Sequence s k

type instance ToPretty (Sequence t v) = "Sequence of" <:> ToPretty t

instance LiteralFamily s k => LiteralFamily (Sequence s k) [k] where
  type instance InRange (Sequence s k) xs = ValidateSequenceLiterals s xs xs

  type instance Pretty (Sequence s k) (bs :: [k]) =
    ToPretty (Sequence s k) <$$--> ToPrettySequenceLiterals s bs

  type instance PrettySeperator (Sequence s k) (     '[]  :: [k]) = 'PrettySpace
  type instance PrettySeperator (Sequence s k) ((x ': xs) :: [k]) = 'PrettyNewline

  type instance SizeOf (Sequence s k) (     '[]  :: [k]) = 0
  type instance SizeOf (Sequence s k) ((x ': xs) :: [k]) = SizeOf s x + SizeOf (Sequence s k) xs

-- | Validate recursivly all sequence members
type family ValidateSequenceLiterals s (all :: [k]) (x :: [k]) :: Bool
  where
    ValidateSequenceLiterals s ls '[] = 'True
    ValidateSequenceLiterals s ls (e ': xs) =
      If (InRange s e)
         (ValidateSequenceLiterals s ls xs)
         (TypeError ('Text "Error while validating the literal sequence of type: "
                     ':<>: 'ShowType s
                     ':<>: 'Text " with contents: "
                     ':<>: 'ShowType ls
                     ':<>: 'Text ", have an invalid element: "
                     ':<>: 'ShowType e))

-- | Prettify recursivly all sequence members
type family ToPrettySequenceLiterals s (x :: [k]) :: PrettyType where
  ToPrettySequenceLiterals s '[] = 'PrettyEmpty
  ToPrettySequenceLiterals s (x ': xs) = Pretty s x <++> PrettySeperator s x <++> ToPrettySequenceLiterals s xs
