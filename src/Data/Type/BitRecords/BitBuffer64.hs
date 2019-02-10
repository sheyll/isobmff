{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.Type.BitRecords.BitBuffer64
    ( type BitBuffer64MaxLength
    , bitBuffer64MaxLength
    , bitBuffer64MaxLengthBytes
    , BitBuffer64()
    , bitBuffer64Content
    , bitBuffer64Length
    , isBitBuffer64Empty
    , bitBuffer64SpaceLeft
    , bitBuffer64
    , emptyBitBuffer64
    , bitBuffer64ProxyLength
    , bufferBits
    , type KnownBufferSize
    ) where

import           Data.Proxy
import           Data.Bits
import           Data.Word
import           Data.Kind ( Constraint )
import           GHC.TypeLits

-- | The maximum number of bits a 'BitBuffer' can hold.
type BitBuffer64MaxLength = 64

-- | The maximum number of bits a 'BitBuffer' can hold.
bitBuffer64MaxLength :: Int
bitBuffer64MaxLength = 64 -- fromInteger (natVal (Proxy :: Proxy BitBuffer64MaxLength))

-- | The maximum number of bytes a 'BitBuffer' can hold.
bitBuffer64MaxLengthBytes :: Word64
bitBuffer64MaxLengthBytes = 8

-- | A buffer for 64 bits, such that the bits are written MSB to LSB.
--
-- > type TwoFields = "f0" @: Field m .+. "f1" @: Field n
--
-- Writes:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)|k-(m+n)  ..  0|
--  Value: |------f0------|--------f1--------|XXXXXXXXXXXXXX|
-- @
--
-- Where @k@ is the current bit offset.
-- The input values are expected to be in the order of the fields, i.e.:
--
-- @
-- toFunction $ bitBuffer64Builder (Proxy :: Proxy TwoFields) 1 2
-- @
--
-- Will result in:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0       ..      10| X    ..      X|
-- @
--
-- The string of bits with a given length (but always @<= 'bitBuffer64MaxLength'@.
-- The number of bits must be smaller that 'bitBuffer64MaxLength'.
data BitBuffer64 = BitBuffer64 !Word64 !Int

bitBuffer64Content :: BitBuffer64 -> Word64
bitBuffer64Content (BitBuffer64 !c _) =
    c

bitBuffer64Length :: BitBuffer64 -> Int
bitBuffer64Length (BitBuffer64 _ !len) =
    len

isBitBuffer64Empty :: BitBuffer64 -> Bool
isBitBuffer64Empty (BitBuffer64 _ !len) =
    len == 0

bitBuffer64SpaceLeft :: BitBuffer64 -> Int
bitBuffer64SpaceLeft (BitBuffer64 _ !len) =
    bitBuffer64MaxLength - len

-- | Create a 'BitBuffer64' containing @len@ bits from LSB to MSB, properly
-- masked, such that only @len@ least significant bits are kept..
bitBuffer64 :: Int -> Word64 -> BitBuffer64
bitBuffer64 !len !b =
    BitBuffer64 (let !s = bitBuffer64MaxLength - len in ((b `unsafeShiftL` s) `unsafeShiftR` s)) len

-- | Create an empty 'BitBuffer64'.
emptyBitBuffer64 :: BitBuffer64
emptyBitBuffer64 = BitBuffer64 0 0

-- | Create a 'BitBuffer64' with a length given by a 'Proxy' to a type level
-- 'Nat'.
bitBuffer64ProxyLength :: (KnownBufferSize n) => Proxy n -> Word64 -> BitBuffer64
bitBuffer64ProxyLength !plen !v = bitBuffer64 fieldLen v
    where
      !fieldLen = fromIntegral (natVal plen)


-- | Copy bits starting at a specific offset from one @a@ the the other.
-- Set bits starting from the most significant bit to the least.
--   For example @writeBits m 1 <> writeBits n 2@ would result in:
--
-- @
--         MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0        ..     10|  ...          |
--          ->             ->                 ->     (direction of writing)
-- @
--
bufferBits :: BitBuffer64 -- ^ The value to write (in the lower @length@ bits).
           -> BitBuffer64 -- ^ The input to write to (starting from length)
           -> (BitBuffer64, BitBuffer64) -- ^ The remaining bits that did not fit
                                         -- in the buffer and the output buffer.
bufferBits (BitBuffer64 !bits !len) (BitBuffer64 !buff !offset) =
    let !spaceAvailable = bitBuffer64MaxLength - offset
        !writeLen = min spaceAvailable len
        !writeOffset = spaceAvailable - writeLen
        !restLen = len - writeLen
        !restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        !buff' = buff .|.
            (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
    in
        (BitBuffer64 restBits restLen, BitBuffer64 buff' (offset + writeLen))

type family KnownBufferSize (s :: Nat) :: Constraint where
        KnownBufferSize size = (KnownNat size, size <= BitBuffer64MaxLength)
