{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import Data.Type.BitRecords.BitBuffer64
import Data.FunctionBuilder
import Data.Type.BitRecords.Core
import Data.Word
import Data.Int
import Data.Bits
import Data.Kind.Extra
import Data.Proxy
import GHC.TypeLits
import Data.Monoid
import Control.Category
import Prelude hiding ((.), id)
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import Text.Printf
import Type.Reflection

-- | A wrapper around a builder derived from a 'BitBuilderState'
data BuilderWithSize where
  MkBuilderWithSize :: !Word64 -> !SB.Builder -> BuilderWithSize

instance Semigroup BuilderWithSize where
  (MkBuilderWithSize !ls !lb) <> (MkBuilderWithSize !rs !rb) =
    MkBuilderWithSize (ls + rs) (lb <> rb)

instance Monoid BuilderWithSize where
  mempty = MkBuilderWithSize 0 mempty

-- | Create a 'SB.Builder' from a 'BitRecord' and store it in a 'BuilderWithSize'
bitBuilderWithSize ::
  forall (record :: BitRecord) . HasFunctionBuilder BitBuilder (Proxy record)
  =>  Proxy record
  -> ToFunction BitBuilder (Proxy record) BuilderWithSize
bitBuilderWithSize = toFunction . builderBoxConstructor

-- | Like 'bitBuilderWithSize', but 'toFunction' the result and accept as an additional
-- parameter a wrapper function to wrap the final result (the 'BuilderWithSize') and
-- 'toFunction' the whole machiner.
wrapBitBuilderWithSize ::
  forall (record :: BitRecord) wrapped .
    HasFunctionBuilder BitBuilder (Proxy record)
  => (BuilderWithSize -> wrapped)
  -> Proxy record
  -> ToFunction BitBuilder (Proxy record) wrapped
wrapBitBuilderWithSize !f !p = toFunction (mapAccumulator f (builderBoxConstructor p))

-- | Create a 'SB.Builder' from a 'BitRecord' and store it in a 'BuilderWithSize';
-- return a 'FunctionBuilder' monoid that does that on 'toFunction'
builderBoxConstructor ::
  forall (record :: BitRecord) r .
  HasFunctionBuilder BitBuilder (Proxy record)
  =>  Proxy record
  -> FunctionBuilder BuilderWithSize r (ToFunction BitBuilder (Proxy record) r)
builderBoxConstructor !p =
  let fromBitBuilder !h =
        let (BitBuilderState !builder _ !wsize) =
              flushBitBuilder
              $ appBitBuilder h initialBitBuilderState
            !out = MkBuilderWithSize wsize builder
        in out
  in mapAccumulator fromBitBuilder (toFunctionBuilder p)

-- * Low-level interface to building 'BitRecord's and other things

newtype BitBuilder =
  BitBuilder {unBitBuilder :: Dual (Endo BitBuilderState)}
  deriving (Monoid, Semigroup)

runBitBuilder
  :: BitBuilder -> SB.Builder
runBitBuilder !w =
  getBitBuilderStateBuilder $
  flushBitBuilder $ appBitBuilder w initialBitBuilderState

bitBuffer64Builder :: (BitBuilderState -> BitBuilderState)
                 -> BitBuilder
bitBuffer64Builder = BitBuilder . Dual . Endo

appBitBuilder :: BitBuilder
                    -> BitBuilderState
                    -> BitBuilderState
appBitBuilder !w = appEndo (getDual (unBitBuilder w))

data BitBuilderState where
        BitBuilderState ::
          !SB.Builder -> !BitBuffer64 -> !Word64 -> BitBuilderState

getBitBuilderStateBuilder
  :: BitBuilderState -> SB.Builder
getBitBuilderStateBuilder (BitBuilderState !builder _ _) = builder

initialBitBuilderState
  :: BitBuilderState
initialBitBuilderState =
  BitBuilderState mempty emptyBitBuffer64 0

-- | Write the partial buffer contents using any number of 'word8' The unwritten
--   parts of the bittr buffer are at the top.  If the
--
-- >     63  ...  (63-off-1)(63-off)  ...  0
-- >     ^^^^^^^^^^^^^^^^^^^
-- > Relevant bits start to the top!
--
flushBitBuilder
  :: BitBuilderState -> BitBuilderState
flushBitBuilder (BitBuilderState !bldr !buff !totalSize) =
  BitBuilderState (writeRestBytes bldr 0)
                        emptyBitBuffer64
                        totalSize'
  where !off = bitBuffer64Length buff
        !off_ = (fromIntegral off :: Word64)
        !totalSize' = totalSize + signum (off_ `rem` 8) + (off_ `div` 8)
        !part = bitBuffer64Content buff
        -- write bytes from msb to lsb until the offset is reached
        -- >  63  ...  (63-off-1)(63-off)  ...  0
        -- >  ^^^^^^^^^^^^^^^^^^^
        -- >  AAAAAAAABBBBBBBBCCC00000
        -- >  |byte A| byte B| byte C|
        writeRestBytes !bldr' !flushOffset =
          if off <= flushOffset
             then bldr'
             else let !flushOffset' = flushOffset + 8
                      !bldr'' =
                        bldr' <>
                        SB.word8 (fromIntegral
                                 ((part `unsafeShiftR`
                                   (bitBuffer64MaxLength - flushOffset')) .&.
                                  0xFF))
                  in writeRestBytes bldr'' flushOffset'

-- | Write all the bits, in chunks, filling and writing the 'BitBuffer64'
-- in the 'BitBuilderState' as often as necessary.
appendBitBuffer64 :: BitBuffer64 -> BitBuilder
appendBitBuffer64 !x' =
  bitBuffer64Builder $
  \(BitBuilderState !builder !buff !totalSizeIn) -> go x' builder buff totalSizeIn
  where go !x !builder !buff !totalSize
          | bitBuffer64Length x == 0 = BitBuilderState builder buff totalSize
          | otherwise =
            let (!rest, !buff') = bufferBits x buff
            in if bitBuffer64SpaceLeft buff' > 0
                  then BitBuilderState builder buff' totalSize
                  else let !nextBuilder =
                             builder <>
                             SB.word64BE (bitBuffer64Content buff')
                           !totalSize' = totalSize + bitBuffer64MaxLengthBytes
                       in go rest nextBuilder emptyBitBuffer64 totalSize'

-- | Write all the b*y*tes, into the 'BitBuilderState' this allows general
-- purposes non-byte aligned builders.
appendStrictByteString :: SB.ByteString -> BitBuilder
appendStrictByteString !sb =
  foldMap (appendBitBuffer64 . bitBuffer64 8 . fromIntegral) (SB.unpack sb)

runBitBuilderHoley
  :: FunctionBuilder BitBuilder SB.Builder a -> a
runBitBuilderHoley (FB !x) = x runBitBuilder

instance HasFunctionBuilder BitBuilder BitBuffer64 where
  toFunctionBuilder = immediate . appendBitBuffer64

-- ** 'BitRecordField' instances

type family UnsignedDemoteRep i where
  UnsignedDemoteRep Int8  = Word8
  UnsignedDemoteRep Int16 = Word16
  UnsignedDemoteRep Int32 = Word32
  UnsignedDemoteRep Int64 = Word64

-- *** BitFields

instance
  forall (nested :: BitField rt st s)  .
   ( HasFunctionBuilder BitBuilder (Proxy nested) )
  => HasFunctionBuilder BitBuilder (Proxy (Konst nested)) where
  type ToFunction BitBuilder (Proxy (Konst nested)) a =
    ToFunction BitBuilder (Proxy nested) a
  toFunctionBuilder _ = toFunctionBuilder (Proxy @nested)


instance
  forall rt (nested :: BitField rt st s) .
   ( DynamicContent BitBuilder (Proxy nested) rt )
  => DynamicContent BitBuilder (Proxy (Konst nested)) rt where
  addParameter _ = addParameter (Proxy @nested)

-- -- *** Labbeled Fields

instance
  forall nested l .
   ( HasFunctionBuilder BitBuilder (Proxy nested) )
  => HasFunctionBuilder BitBuilder (Proxy (LabelF l nested)) where
  type ToFunction BitBuilder (Proxy (LabelF l nested)) a =
    ToFunction BitBuilder (Proxy nested) a
  toFunctionBuilder _ = toFunctionBuilder (Proxy @nested)

instance ( DynamicContent BitBuilder (Proxy nested) b )
  => DynamicContent BitBuilder (Proxy (LabelF l nested)) b where
  addParameter _ = addParameter (Proxy @nested)

instance
  forall (nested :: Extends (BitField rt st s)) l .
   ( HasFunctionBuilder BitBuilder (Proxy nested) )
  => HasFunctionBuilder BitBuilder (Proxy (Labelled l nested)) where
  type ToFunction BitBuilder (Proxy (Labelled l nested)) a =
    ToFunction BitBuilder (Proxy nested) a
  toFunctionBuilder _ = toFunctionBuilder (Proxy @nested)

instance
  forall rt st s (nested :: Extends (BitField rt st s)) (l :: Symbol) out .
   ( DynamicContent BitBuilder (Proxy nested) rt )
  => DynamicContent BitBuilder (Proxy (Labelled l nested)) rt where
  addParameter _ = addParameter (Proxy @nested)

-- -- **** Bool

instance forall f . (FieldWidth f ~ 1) =>
  HasFunctionBuilder BitBuilder (Proxy (f := 'True)) where
  toFunctionBuilder _ = immediate (appendBitBuffer64 (bitBuffer64 1 1))

instance forall f . (FieldWidth f ~ 1) =>
  HasFunctionBuilder BitBuilder (Proxy (f := 'False)) where
  toFunctionBuilder _ = immediate (appendBitBuffer64 (bitBuffer64 1 0))

instance HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldFlag)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldFlag)) a = Bool -> a
  toFunctionBuilder _ =
    deferred (appendBitBuffer64 . bitBuffer64 1 . (\ !t -> if t then 1 else 0))

-- -- new:

instance forall f . (BitFieldSize (From f) ~ 1) =>
  HasFunctionBuilder BitBuilder (Proxy (f :=. 'True)) where
  toFunctionBuilder _ = immediate (appendBitBuffer64 (bitBuffer64 1 1))

instance forall f . (BitFieldSize (From f) ~ 1) =>
  HasFunctionBuilder BitBuilder (Proxy (f :=. 'False)) where
  toFunctionBuilder _ = immediate (appendBitBuffer64 (bitBuffer64 1 0))

instance HasFunctionBuilder BitBuilder (Proxy 'MkFieldFlag) where
  type ToFunction BitBuilder (Proxy 'MkFieldFlag) a = Bool -> a
  toFunctionBuilder _ =
    deferred (appendBitBuffer64 . bitBuffer64 1 . (\ !t -> if t then 1 else 0))

instance DynamicContent BitBuilder (Proxy 'MkFieldFlag) Bool where
  addParameter = toFunctionBuilder

-- -- **** Bits

instance forall (s :: Nat) . (KnownChunkSize s) =>
  HasFunctionBuilder BitBuilder (Proxy (MkField ('MkFieldBits :: BitField (B s) Nat s))) where
  type ToFunction BitBuilder (Proxy (MkField ('MkFieldBits :: BitField (B s) Nat s))) a = B s -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64ProxyLength (Proxy @s) . unB)


instance forall (s :: Nat) r . (KnownChunkSize s) =>
  DynamicContent BitBuilder (Proxy (MkField ('MkFieldBits :: BitField (B s) Nat s))) (B s) where
  addParameter = toFunctionBuilder

-- -- **** Naturals

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldU64)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldU64)) a = Word64 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 64)

instance
  DynamicContent BitBuilder (Proxy (MkField 'MkFieldU64)) Word64 where
  addParameter = toFunctionBuilder

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldU32)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldU32)) a = Word32 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 32 . fromIntegral)

instance
  DynamicContent BitBuilder (Proxy (MkField ('MkFieldU32:: BitField Word32 Nat 32))) Word32 where
  addParameter = toFunctionBuilder

instance
  HasFunctionBuilder BitBuilder (Proxy 'MkFieldU32) where
  type ToFunction BitBuilder (Proxy 'MkFieldU32) a = Word32 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 32 . fromIntegral)

instance
  DynamicContent BitBuilder (Proxy 'MkFieldU32) Word32 where
  addParameter = toFunctionBuilder

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldU16)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldU16)) a = Word16 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 16 . fromIntegral)

instance
  DynamicContent BitBuilder (Proxy (MkField 'MkFieldU16)) Word16 where
  addParameter = toFunctionBuilder

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldU8)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldU8)) a = Word8 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 8 . fromIntegral)

instance
  DynamicContent BitBuilder (Proxy (MkField 'MkFieldU8)) Word8 where
  addParameter = toFunctionBuilder

-- -- **** Signed

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldI64)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldI64)) a = Int64 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 64 . fromIntegral @Int64 @Word64)

instance
  DynamicContent BitBuilder (Proxy (MkField 'MkFieldI64)) Int64 where
  addParameter = toFunctionBuilder

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldI32)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldI32)) a = Int32 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 32 . fromIntegral . fromIntegral @Int32 @Word32)

instance
  DynamicContent BitBuilder (Proxy (MkField 'MkFieldI32)) Int32 where
  addParameter = toFunctionBuilder

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldI16)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldI16)) a = Int16 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 16 . fromIntegral . fromIntegral @Int16 @Word16)

instance
  DynamicContent BitBuilder (Proxy (MkField 'MkFieldI16)) Int16 where
  addParameter = toFunctionBuilder

instance
  HasFunctionBuilder BitBuilder (Proxy (MkField 'MkFieldI8)) where
  type ToFunction BitBuilder (Proxy (MkField 'MkFieldI8)) a = Int8 -> a
  toFunctionBuilder _ = deferred (appendBitBuffer64 . bitBuffer64 8 . fromIntegral . fromIntegral @Int8 @Word8)

instance
  DynamicContent BitBuilder (Proxy (MkField 'MkFieldI8)) Int8 where
  addParameter = toFunctionBuilder

-- *** Assign static values

instance
  forall
    rt
    (len :: Nat)
    (t :: BitField rt Nat len)
    (f :: Extends (BitRecordField t))
    (v :: Nat)
    .
      ( KnownNat v
      , DynamicContent BitBuilder (Proxy f) rt
      , Num rt
      )
      =>  HasFunctionBuilder BitBuilder (Proxy (f := v)) where
  toFunctionBuilder _ =
    fillParameter (addParameter (Proxy @f)) (fromIntegral (natVal (Proxy @v)))

instance forall v f x . (KnownNat v, DynamicContent BitBuilder (Proxy f) x, Num x) =>
  HasFunctionBuilder BitBuilder (Proxy (f := ('PositiveNat v))) where
  toFunctionBuilder _ =
    fillParameter (addParameter (Proxy @f)) (fromIntegral (natVal (Proxy @v)))


instance forall v f x . (KnownNat v, DynamicContent BitBuilder (Proxy f) x, Num x) =>
  HasFunctionBuilder BitBuilder (Proxy (f := ('NegativeNat v))) where
  toFunctionBuilder _ = fillParameter (addParameter (Proxy @f)) (fromIntegral (-1 * (natVal (Proxy @v))))

-- new:

instance
  forall (f :: Extends (BitField rt Nat len)) (v :: Nat) .
  ( KnownNat v
  , DynamicContent BitBuilder (Proxy f) rt
  , Num rt)
  =>
  HasFunctionBuilder BitBuilder (Proxy (f :=. v)) where
  toFunctionBuilder _ =
    fillParameter
      (addParameter (Proxy @f))
      (fromIntegral (natVal (Proxy @v)))

-- -- instance forall v f a x . (KnownNat v, HasFunctionBuilder BitBuilder (Proxy f) a, ToFunction BitBuilder (Proxy f) a ~ (x -> a), Num x) =>
-- --   HasFunctionBuilder BitBuilder (Proxy (f := ('PositiveNat v))) a where
-- --   toFunctionBuilder _ =  fillParameter (toFunctionBuilder (Proxy @f)) (fromIntegral (natVal (Proxy @v)))
--
--
-- -- instance forall v f a x . (KnownNat v, HasFunctionBuilder BitBuilder (Proxy f) a, ToFunction BitBuilder (Proxy f) a ~ (x -> a), Num x) =>
-- --   HasFunctionBuilder BitBuilder (Proxy (f := ('NegativeNat v))) a where
-- --   toFunctionBuilder _ = fillParameter (toFunctionBuilder (Proxy @f)) (fromIntegral (-1 * (natVal (Proxy @v))))


-- ** 'BitRecord' instances

instance forall (r :: Extends BitRecord) . HasFunctionBuilder BitBuilder (Proxy (From r)) =>
  HasFunctionBuilder BitBuilder (Proxy r) where
  type ToFunction BitBuilder (Proxy r) a =
    ToFunction BitBuilder (Proxy (From r)) a
  toFunctionBuilder _ = toFunctionBuilder (Proxy @(From r))

-- *** 'BitRecordMember'

instance forall f . HasFunctionBuilder BitBuilder (Proxy f) => HasFunctionBuilder BitBuilder (Proxy ('BitRecordMember f)) where
  type ToFunction BitBuilder (Proxy ('BitRecordMember f)) a = ToFunction BitBuilder (Proxy f) a
  toFunctionBuilder _ = toFunctionBuilder (Proxy @f)

-- *** 'RecordField'


instance forall f . HasFunctionBuilder BitBuilder (Proxy f)
  => HasFunctionBuilder BitBuilder (Proxy ('RecordField f)) where
  type ToFunction BitBuilder (Proxy ('RecordField f)) a =
        ToFunction BitBuilder (Proxy f) a
  toFunctionBuilder _ = toFunctionBuilder (Proxy @f)


-- *** 'AppendedBitRecords'

instance forall l r .
  (HasFunctionBuilder BitBuilder (Proxy l), HasFunctionBuilder BitBuilder (Proxy r))
   => HasFunctionBuilder BitBuilder (Proxy ('BitRecordAppend l r)) where
  type ToFunction BitBuilder (Proxy ('BitRecordAppend l r)) a =
    ToFunction BitBuilder (Proxy l) (ToFunction BitBuilder (Proxy r) a)
  toFunctionBuilder _ = toFunctionBuilder (Proxy @l) . toFunctionBuilder (Proxy @r)

-- *** 'EmptyBitRecord' and '...Pretty'

instance HasFunctionBuilder BitBuilder (Proxy 'EmptyBitRecord) where
  toFunctionBuilder _ = id

-- ** Tracing/Debug Printing

-- | Print a 'SB.Builder' to a space seperated series of hexa-decimal bytes.
printBuilder :: SB.Builder -> String
printBuilder b =
  ("<< " ++) $
  (++ " >>") $ unwords $ printf "%0.2x" <$> B.unpack (SB.toLazyByteString b)

bitBuffer64Printer :: HasFunctionBuilder BitBuilder a => a -> ToFunction BitBuilder a String
bitBuffer64Printer =
  toFunction . mapAccumulator (printBuilder . runBitBuilder) . toFunctionBuilder
