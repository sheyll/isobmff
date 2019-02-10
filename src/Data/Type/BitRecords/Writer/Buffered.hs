-- | A wrapper around other serialization mechanisms, such as ByteString Builder,
-- or cereal, which only need to provide a singe abstraction, to write
-- full buffers of fixed length of 64bit, and a partially filled last buffer.
--
-- There are two facilities this module offers:
-- 1. The 'ToBuffer' type class and 'Buffer' data type for rendering 'Structure's and type literals
-- 2. The 'BinaryWriter' type class for rendering the stuff vial cereal or Binary, etc...
module Data.Type.BitRecords.Writer.Buffered
  ()
where

import Data.Word


newtype Writer m = MkWriter { runWriter :: Dual (Endo (State m)) }
  deriving (Semigroup, Monoid)

data State m = MkState { _buffer :: Word64, _offset :: Word64, _writer :: m }



-- | Type that allow writing of 'Buffer'
class BinaryWriter m where
  -- | Consume the byte
  handleWord8    :: Word8 -> m
  -- | Consume the 64 bit word (bit endian)
  handleWord64BE :: Word64 -> m
  -- | Consume the 64 bit word (little endian)
  handleWord64LE :: Word64 -> m
