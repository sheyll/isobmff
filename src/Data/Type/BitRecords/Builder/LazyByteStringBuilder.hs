{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}
module Data.Type.BitRecords.Builder.LazyByteStringBuilder
        ( BitBuilder
        , execBitBuilder
        , writeBits
        , appendBitBuffer64
        , appendStrictByteString
        , BuilderWithSize (..)
        , getByteStringBuilder
        , toBuilderWithSizeConstructor
        , toLazyByteStringConstructor
        ) where

import Data.Type.BitRecords.BitBuffer64
import Data.FunctionBuilder
import Data.Word
import Data.Bits
import Data.Monoid
import Control.Category
import Prelude hiding ((.), id)
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB


-- | A wrapper around a builder derived from a 'BitBuilderState'
data BuilderWithSize where
  MkBuilderWithSize :: !Word64 -> !SB.Builder -> BuilderWithSize

getByteStringBuilder :: BuilderWithSize -> SB.Builder
getByteStringBuilder (MkBuilderWithSize _ !b) = b

instance Semigroup BuilderWithSize where
  (MkBuilderWithSize !ls !lb) <> (MkBuilderWithSize !rs !rb) =
    MkBuilderWithSize (ls + rs) (lb <> rb)

instance Monoid BuilderWithSize where
  mempty = MkBuilderWithSize 0 mempty

-- * Low-level interface to building 'BitRecord's and other things


data BitBuilderState where
        BitBuilderState ::
          !SB.Builder -> !BitBuffer64 -> !Word64 -> BitBuilderState

newtype BitBuilder =
  BitBuilder {unBitBuilder :: Dual (Endo BitBuilderState)}
  deriving (Monoid, Semigroup)

execBitBuilder :: BitBuilder -> BuilderWithSize
execBitBuilder !w =
  case flushBitBuilder (appEndo (getDual (unBitBuilder w)) initialBitBuilderState) of
    (BitBuilderState !builder _ !wsize) -> MkBuilderWithSize wsize builder
 where
  initialBitBuilderState =
    BitBuilderState mempty emptyBitBuffer64 0


writeBits :: BitBuilder -> LB.ByteString
writeBits !w =
    case flushBitBuilder (appEndo (getDual (unBitBuilder w)) initialBitBuilderState) of
      (BitBuilderState !builder _ _) ->  SB.toLazyByteString builder
 where
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
  BitBuilder . Dual . Endo $
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


instance HasFunctionBuilder BitBuilder BitBuffer64 where
  toFunctionBuilder = immediate . appendBitBuffer64







-- | Create a 'SB.Builder' and store it in a 'BuilderWithSize'
toBuilderWithSizeConstructor
  :: HasFunctionBuilder BitBuilder a
  => a -> ToFunction BitBuilder a BuilderWithSize
toBuilderWithSizeConstructor = toFunction . mapAccumulator execBitBuilder . toFunctionBuilder

-- | Create a 'SB.Builder' from a 'BitRecord' and store it in a 'BuilderWithSize'
toLazyByteStringConstructor
  :: HasFunctionBuilder BitBuilder a
  => a -> ToFunction BitBuilder a LB.ByteString
toLazyByteStringConstructor = toFunction . mapAccumulator writeBits . toFunctionBuilder
