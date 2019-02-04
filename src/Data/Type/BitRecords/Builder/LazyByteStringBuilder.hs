{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder where

import Data.Type.BitRecords.Builder.BitBuffer64
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
  forall (record :: BitRecord) . HasBitBuilder (Proxy record)
  =>  Proxy record
  -> ToBitBuilder (Proxy record) BuilderWithSize
bitBuilderWithSize = toFunction . builderBoxConstructor

-- | Like 'bitBuilderWithSize', but 'toFunction' the result and accept as an additional
-- parameter a wrapper function to wrap the final result (the 'BuilderWithSize') and
-- 'toFunction' the whole machiner.
wrapBitBuilderWithSize ::
  forall (record :: BitRecord) wrapped .
    HasBitBuilder (Proxy record)
  => (BuilderWithSize -> wrapped)
  -> Proxy record
  -> ToBitBuilder (Proxy record) wrapped
wrapBitBuilderWithSize !f !p = toFunction (mapAccumulator f (builderBoxConstructor p))

-- | Create a 'SB.Builder' from a 'BitRecord' and store it in a 'BuilderWithSize';
-- return a 'FunctionBuilder' monoid that does that on 'toFunction'
builderBoxConstructor ::
  forall (record :: BitRecord) r .
  HasBitBuilder (Proxy record)
  =>  Proxy record
  -> FunctionBuilder BuilderWithSize r (ToBitBuilder (Proxy record) r)
builderBoxConstructor !p =
  let fromBitBuilder !h =
        let (BitBuilderState !builder _ !wsize) =
              flushBitBuilder
              $ appBitBuilder h initialBitBuilderState
            !out = MkBuilderWithSize wsize builder
        in out
  in mapAccumulator fromBitBuilder (bitBuffer64BuilderHoley p)

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

-- * 'BitBuffer64' construction from 'BitRecord's

class HasBitBuilder a where
  type ToBitBuilder a r
  type ToBitBuilder a r = r
  bitBuffer64BuilderHoley :: a -> FunctionBuilder BitBuilder r (ToBitBuilder a r)

instance HasBitBuilder BitBuffer64 where
  bitBuffer64BuilderHoley = immediate . appendBitBuffer64

-- ** 'BitRecordField' instances

type family UnsignedDemoteRep i where
  UnsignedDemoteRep Int8  = Word8
  UnsignedDemoteRep Int16 = Word16
  UnsignedDemoteRep Int32 = Word32
  UnsignedDemoteRep Int64 = Word64

-- *** BitFields

instance
  forall (nested :: BitField rt st s)  .
   ( HasBitBuilder (Proxy nested) )
  => HasBitBuilder (Proxy (Konst nested)) where
  type ToBitBuilder (Proxy (Konst nested)) a =
    ToBitBuilder (Proxy nested) a
  bitBuffer64BuilderHoley _ = bitBuffer64BuilderHoley (Proxy @nested)


-- -- *** Labbeled Fields

instance
  forall nested l .
   ( HasBitBuilder (Proxy nested) )
  => HasBitBuilder (Proxy (LabelF l nested)) where
  type ToBitBuilder (Proxy (LabelF l nested)) a =
    ToBitBuilder (Proxy nested) a
  bitBuffer64BuilderHoley _ = bitBuffer64BuilderHoley (Proxy @nested)

instance
  forall (nested :: Extends (BitField rt st s)) l .
   ( HasBitBuilder (Proxy nested) )
  => HasBitBuilder (Proxy (Labelled l nested)) where
  type ToBitBuilder (Proxy (Labelled l nested)) a =
    ToBitBuilder (Proxy nested) a
  bitBuffer64BuilderHoley _ = bitBuffer64BuilderHoley (Proxy @nested)

-- -- **** Bool

instance forall f . (FieldWidth f ~ 1) =>
  HasBitBuilder (Proxy (f := 'True)) where
  bitBuffer64BuilderHoley _ = immediate (appendBitBuffer64 (bitBuffer64 1 1))

instance forall f . (FieldWidth f ~ 1) =>
  HasBitBuilder (Proxy (f := 'False)) where
  bitBuffer64BuilderHoley _ = immediate (appendBitBuffer64 (bitBuffer64 1 0))

instance HasBitBuilder (Proxy (MkField 'MkFieldFlag)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldFlag)) a = Bool -> a
  bitBuffer64BuilderHoley _ =
    addParameter (appendBitBuffer64 . bitBuffer64 1 . (\ !t -> if t then 1 else 0))

-- -- new:

instance forall f . (BitFieldSize (From f) ~ 1) =>
  HasBitBuilder (Proxy (f :=. 'True)) where
  bitBuffer64BuilderHoley _ = immediate (appendBitBuffer64 (bitBuffer64 1 1))

instance forall f . (BitFieldSize (From f) ~ 1) =>
  HasBitBuilder (Proxy (f :=. 'False)) where
  bitBuffer64BuilderHoley _ = immediate (appendBitBuffer64 (bitBuffer64 1 0))

instance HasBitBuilder (Proxy 'MkFieldFlag) where
  type ToBitBuilder (Proxy 'MkFieldFlag) a = Bool -> a
  bitBuffer64BuilderHoley _ =
    addParameter (appendBitBuffer64 . bitBuffer64 1 . (\ !t -> if t then 1 else 0))

-- -- **** Bits

instance forall (s :: Nat) . (KnownChunkSize s) =>
  HasBitBuilder (Proxy (MkField ('MkFieldBits :: BitField (B s) Nat s))) where
  type ToBitBuilder (Proxy (MkField ('MkFieldBits :: BitField (B s) Nat s))) a = B s -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64ProxyLength (Proxy @s) . unB)

-- -- **** Naturals

instance
  HasBitBuilder (Proxy (MkField 'MkFieldU64)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldU64)) a = Word64 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 64)

instance
  HasBitBuilder (Proxy (MkField 'MkFieldU32)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldU32)) a = Word32 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 32 . fromIntegral)

instance
  HasBitBuilder (Proxy 'MkFieldU32) where
  type ToBitBuilder (Proxy 'MkFieldU32) a = Word32 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 32 . fromIntegral)

instance
  HasBitBuilder (Proxy (MkField 'MkFieldU16)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldU16)) a = Word16 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 16 . fromIntegral)

instance
  HasBitBuilder (Proxy (MkField 'MkFieldU8)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldU8)) a = Word8 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 8 . fromIntegral)

-- -- **** Signed

instance
  HasBitBuilder (Proxy (MkField 'MkFieldI64)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldI64)) a = Int64 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 64 . fromIntegral @Int64 @Word64)

instance
  HasBitBuilder (Proxy (MkField 'MkFieldI32)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldI32)) a = Int32 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 32 . fromIntegral . fromIntegral @Int32 @Word32)

instance
  HasBitBuilder (Proxy (MkField 'MkFieldI16)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldI16)) a = Int16 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 16 . fromIntegral . fromIntegral @Int16 @Word16)

instance
  HasBitBuilder (Proxy (MkField 'MkFieldI8)) where
  type ToBitBuilder (Proxy (MkField 'MkFieldI8)) a = Int8 -> a
  bitBuffer64BuilderHoley _ = addParameter (appendBitBuffer64 . bitBuffer64 8 . fromIntegral . fromIntegral @Int8 @Word8)

-- *** Assign static values

instance
  forall
    rt
    len
    (t :: BitField rt Nat len)
    (f :: Extends (BitRecordField t))
    (v :: Nat)
    .
      ( KnownNat v
      , forall a . (Num rt, HasBitBuilder (Proxy f), ToBitBuilder (Proxy f) a ~ (rt -> a))
      )
      =>  HasBitBuilder (Proxy (f := v)) where
  bitBuffer64BuilderHoley _ = fillParameter (bitBuffer64BuilderHoley (Proxy @f)) (fromIntegral (natVal (Proxy @v)))

instance forall v f x . (KnownNat v, HasBitBuilder (Proxy f), forall a . (ToBitBuilder (Proxy f) a ~ (x -> a)), Num x) =>
  HasBitBuilder (Proxy (f := ('PositiveNat v))) where
  bitBuffer64BuilderHoley _ =  fillParameter (bitBuffer64BuilderHoley (Proxy @f)) (fromIntegral (natVal (Proxy @v)))


instance forall v f x . (KnownNat v, HasBitBuilder (Proxy f), forall a . (ToBitBuilder (Proxy f) a ~ (x -> a)), Num x) =>
  HasBitBuilder (Proxy (f := ('NegativeNat v))) where
  bitBuffer64BuilderHoley _ = fillParameter (bitBuffer64BuilderHoley (Proxy @f)) (fromIntegral (-1 * (natVal (Proxy @v))))

-- new:

instance
  forall (f :: Extends (BitField rt Nat len)) (v :: Nat) .
  ( KnownNat v
  , HasBitBuilder (Proxy f)
  , forall a . (ToBitBuilder (Proxy f) a ~ (rt -> a))
  , Num rt)
  =>
  HasBitBuilder (Proxy (f :=. v)) where
  bitBuffer64BuilderHoley _ =
    fillParameter
      (bitBuffer64BuilderHoley (Proxy @f))
      (fromIntegral (natVal (Proxy @v)))

-- -- instance forall v f a x . (KnownNat v, HasBitBuilder (Proxy f) a, ToBitBuilder (Proxy f) a ~ (x -> a), Num x) =>
-- --   HasBitBuilder (Proxy (f := ('PositiveNat v))) a where
-- --   bitBuffer64BuilderHoley _ =  fillParameter (bitBuffer64BuilderHoley (Proxy @f)) (fromIntegral (natVal (Proxy @v)))
--
--
-- -- instance forall v f a x . (KnownNat v, HasBitBuilder (Proxy f) a, ToBitBuilder (Proxy f) a ~ (x -> a), Num x) =>
-- --   HasBitBuilder (Proxy (f := ('NegativeNat v))) a where
-- --   bitBuffer64BuilderHoley _ = fillParameter (bitBuffer64BuilderHoley (Proxy @f)) (fromIntegral (-1 * (natVal (Proxy @v))))


-- ** 'BitRecord' instances

instance forall (r :: Extends BitRecord) . HasBitBuilder (Proxy (From r)) =>
  HasBitBuilder (Proxy r) where
  type ToBitBuilder (Proxy r) a =
    ToBitBuilder (Proxy (From r)) a
  bitBuffer64BuilderHoley _ = bitBuffer64BuilderHoley (Proxy @(From r))

-- *** 'BitRecordMember'

instance forall f . HasBitBuilder (Proxy f) => HasBitBuilder (Proxy ('BitRecordMember f)) where
  type ToBitBuilder (Proxy ('BitRecordMember f)) a = ToBitBuilder (Proxy f) a
  bitBuffer64BuilderHoley _ = bitBuffer64BuilderHoley (Proxy @f)

-- *** 'RecordField'


instance forall f . HasBitBuilder (Proxy f)
  => HasBitBuilder (Proxy ('RecordField f)) where
  type ToBitBuilder (Proxy ('RecordField f)) a =
        ToBitBuilder (Proxy f) a
  bitBuffer64BuilderHoley _ = bitBuffer64BuilderHoley (Proxy @f)


-- *** 'AppendedBitRecords'

instance forall l r .
  (HasBitBuilder (Proxy l), HasBitBuilder (Proxy r))
   => HasBitBuilder (Proxy ('BitRecordAppend l r)) where
  type ToBitBuilder (Proxy ('BitRecordAppend l r)) a =
    ToBitBuilder (Proxy l) (ToBitBuilder (Proxy r) a)
  bitBuffer64BuilderHoley _ = bitBuffer64BuilderHoley (Proxy @l) . bitBuffer64BuilderHoley (Proxy @r)

-- *** 'EmptyBitRecord' and '...Pretty'

instance HasBitBuilder (Proxy 'EmptyBitRecord) where
  bitBuffer64BuilderHoley _ = id

-- ** Tracing/Debug Printing

-- | Print a 'SB.Builder' to a space seperated series of hexa-decimal bytes.
printBuilder :: SB.Builder -> String
printBuilder b =
  ("<< " ++) $
  (++ " >>") $ unwords $ printf "%0.2x" <$> B.unpack (SB.toLazyByteString b)

bitBuffer64Printer :: HasBitBuilder a => a -> ToBitBuilder a String
bitBuffer64Printer =
  toFunction . mapAccumulator (printBuilder . runBitBuilder) . bitBuffer64BuilderHoley
