{-# LANGUAGE UndecidableInstances #-}
module Data.Type.BitRecords.Structure.Constructor
        ()
where

import           Data.Int
import           Data.Kind.Extra
import           Data.Kind
import           Data.Word
import           Data.Type.BitRecords.Structure
import           GHC.TypeLits
import qualified Data.Type.BitRecords.Structure.TypeLits
                                               as Literal


-- | The class accompanying 'Structure' derivatives
-- | The type signature of a constructor (function) for a structure
type family Constructor (t :: Extends (Structure sizeType)) next :: k

type instance Constructor EmptyStructure next = next
type instance Constructor (Anonymous (Name name struct)) next = Constructor struct next
type instance Constructor (Record ('[] :: [Extends (Named (Structure sizeType))])) next = next
type instance Constructor (Record (x ': xs)) next = Constructor (Anonymous x) (Constructor (Record xs) next)
type instance Constructor (BitSequence length) next = WithValidBitSequenceLength length (Word64 -> next)
type instance Constructor (TypeStructure Bool) next = Bool -> next
type instance Constructor (TypeStructure Int8) next = Int8 -> next
type instance Constructor (LiteralStructure r) next = next
type instance Constructor (TypeStructure Word8) next = Word8 -> next
type instance Constructor (IntegerStructure n s e) next   =
  IntegerStructureValidateLength n (IntegerStructure n s e 'MkFixStructure -> next)
type instance Constructor (Assign s a) next = AssignStructureValidateSize s a next
type instance Constructor (ConditionalStructure 'True l r) next = Constructor l next
type instance Constructor (ConditionalStructure 'False l r) next = Constructor r next
type instance Constructor (AnyStructure (c :: Extends (Structure sizeType))) next = Constructor c next
type instance Constructor (AnyStructure (c :: Type) ) next = c -> next

_constructorSpec :: ()
_constructorSpec =
        (undefined :: Constructor U8 ()) (undefined :: Word8)
                <> (undefined :: Constructor S8 ()) (undefined :: Int8)
                <> (undefined :: Constructor FlagStructure ())
                           (undefined :: Bool)
                <> (undefined :: Constructor (S 16 'BE) ())
                           (undefined :: IntegerStructure
                                     16
                                     'Signed
                                     'BE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (U 16 'BE) ())
                           (undefined :: IntegerStructure
                                     16
                                     'Unsigned
                                     'BE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (S 16 'LE) ())
                           (undefined :: IntegerStructure
                                     16
                                     'Signed
                                     'LE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (U 16 'LE) ())
                           (undefined :: IntegerStructure
                                     16
                                     'Unsigned
                                     'LE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (S 32 'BE) ())
                           (undefined :: IntegerStructure
                                     32
                                     'Signed
                                     'BE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (U 32 'BE) ())
                           (undefined :: IntegerStructure
                                     32
                                     'Unsigned
                                     'BE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (S 32 'LE) ())
                           (undefined :: IntegerStructure
                                     32
                                     'Signed
                                     'LE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (U 32 'LE) ())
                           (undefined :: IntegerStructure
                                     32
                                     'Unsigned
                                     'LE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (S 64 'BE) ())
                           (undefined :: IntegerStructure
                                     64
                                     'Signed
                                     'BE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (U 64 'BE) ())
                           (undefined :: IntegerStructure
                                     64
                                     'Unsigned
                                     'BE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (S 64 'LE) ())
                           (undefined :: IntegerStructure
                                     64
                                     'Signed
                                     'LE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (U 64 'LE) ())
                           (undefined :: IntegerStructure
                                     64
                                     'Unsigned
                                     'LE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (Anonymous (Name "foo" U8)) ())
                           (undefined :: Word8)
                <> (undefined :: Constructor
                             ( Assign
                                       ( Name "foo" U8 <> Name "bar" FlagStructure
                                       )
                                       (LiteralStructure (Literal.To Nat 256))
                             )
                             ()
                   )
                <> (undefined :: Constructor (Record '[]) ())
                <> (undefined :: Constructor EmptyStructure ())
                <> (undefined :: Constructor (BitSequence 15) ())
                           (undefined :: Word64)
                <> (undefined :: Constructor
                             (ConditionalStructure 'True (Record '[]) U8)
                             ()
                   )
                <> (undefined :: Constructor
                             (ConditionalStructure 'False (Record '[]) U8)
                             ()
                   )
                           (undefined :: Word8)
                <> (undefined :: Constructor
                             (Record '[Name "x" U8, Name "y" S8])
                             ()
                   )
                           (undefined :: Word8)
                           (undefined :: Int8)
                <> (undefined :: Constructor
                             (Name "x" U8 <> Name "y" S8 <> Name "z" S8)
                             ()
                   )
                           (undefined :: Word8)
                           (undefined :: Int8)
                           (undefined :: Int8)
                <> (undefined :: Constructor
                             (LiteralStructure (Literal.Bits '[1, 0, 1, 0]))
                             ()
                   )
                <> (undefined :: Constructor (AnyStructure (U 64 'BE)) ())
                           (undefined :: IntegerStructure
                                     64
                                     'Unsigned
                                     'BE
                                     'MkFixStructure
                           )
                <> (undefined :: Constructor (AnyStructure Word64) ()) (42)
