-- | A wrapper around other serialization mechanisms, such as ByteString Builder,
-- or cereal, which only need to provide a singe abstraction, to write
-- full buffers of fixed length of 64bit, and a partially filled last buffer.
module Data.Type.BitRecords.Writer.Buffered () where


