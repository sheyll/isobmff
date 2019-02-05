{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder2 where

import Data.Type.BitRecords.Builder.BitBuffer64
import Data.FunctionBuilder
import Data.Type.BitRecords.Structure
import Data.Word
import Data.Bits
import Data.Kind.Extra
import Data.Proxy
import Data.Monoid
import Control.Category
import Prelude hiding ((.), id)
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB
import Text.Printf

-- * 'BitBuffer64' construction from 'Extends Structure's

class HasFunctionBuilder w a where
  type ToFunction w a r
  type ToFunction w a r = r
  toFunctionBuilder :: a -> FunctionBuilder w r (ToFunction w a r)

class (HasFunctionBuilder w a)
   =>  HasFunctionBuilder1 w a b | w a -> b where
  toParametricFunctionBuilder :: a -> FunctionBuilder w r (b -> r)

instance HasFunctionBuilder BitBuilder BitBuffer64 where
  toFunctionBuilder = immediate . appendBitBuffer64

newtype BitBuilder =
  BitBuilder {unBitBuilder :: Dual (Endo BitBuilderState)}
  deriving (Monoid, Semigroup)

data BitBuilderState where
        BitBuilderState ::
          !SB.Builder -> !BitBuffer64 -> !Word64 -> BitBuilderState

-- | A wrapper around a builder derived from a 'BitBuilderState'
data BuilderWithSize where
  MkBuilderWithSize :: !Word64 -> !SB.Builder -> BuilderWithSize

instance Semigroup BuilderWithSize where
  (MkBuilderWithSize !ls !lb) <> (MkBuilderWithSize !rs !rb) =
    MkBuilderWithSize (ls + rs) (lb <> rb)

instance Monoid BuilderWithSize where
  mempty = MkBuilderWithSize 0 mempty

-- | Create a 'SB.Builder' from a 'Structure' and store it in a 'BuilderWithSize'
bitBuilderWithSize ::
  forall (struct :: Extends Structure) .
  HasFunctionBuilder BitBuilder (Proxy struct)
  => Proxy struct
  -> ToFunction BitBuilder (Proxy struct) BuilderWithSize
bitBuilderWithSize = toFunction . builderBoxConstructor

-- | Like 'bitBuilderWithSize', but 'toFunction' the result and accept as an additional
-- parameter a wrapper function to wrap the final result (the 'BuilderWithSize') and
-- 'toFunction' the whole machiner.
wrapBitBuilderWithSize ::
  forall (struct :: Extends Structure) wrapped .
    HasFunctionBuilder BitBuilder (Proxy struct)
  => (BuilderWithSize -> wrapped)
  -> Proxy struct
  -> ToFunction BitBuilder (Proxy struct) wrapped
wrapBitBuilderWithSize !f !p = toFunction (mapAccumulator f (builderBoxConstructor p))

-- | Create a 'SB.Builder' from a 'Extends Structure' and store it in a 'BuilderWithSize';
-- return a 'FunctionBuilder' monoid that does that on 'toFunction'
builderBoxConstructor ::
  forall (struct :: Extends Structure) r .
  HasFunctionBuilder BitBuilder (Proxy struct)
  =>  Proxy struct
  -> FunctionBuilder BuilderWithSize r (ToFunction BitBuilder (Proxy struct) r)
builderBoxConstructor !p =
  let fromBitBuilder !h =
        let (BitBuilderState !builder _ !wsize) =
              flushBitBuilder
              $ appBitBuilder h initialBitBuilderState
            !out = MkBuilderWithSize wsize builder
        in out
  in mapAccumulator fromBitBuilder (addParameter p)

-- * Low-level interface to building 'Extends Structure's and other things
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


-- ** Tracing/Debug Printing

-- | Print a 'SB.Builder' to a space seperated series of hexa-decimal bytes.
printBuilder :: SB.Builder -> String
printBuilder b =
  ("<< " ++) $
  (++ " >>") $ unwords $ printf "%0.2x" <$> B.unpack (SB.toLazyByteString b)

bitBuffer64Printer :: HasFunctionBuilder BitBuilder a => a -> ToFunction BitBuilder a String
bitBuffer64Printer =
  toFunction . mapAccumulator (printBuilder . runBitBuilder) . addParameter
