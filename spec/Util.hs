module Util where

import qualified Data.ByteString.Builder       as SB
import           Text.Printf
import qualified Data.ByteString.Lazy          as B
import           Data.FunctionBuilder
import           Data.Type.BitRecords.Builder.LazyByteStringBuilder
import           Test.QuickCheck
import           GHC.TypeLits
import           Data.Type.Bool                 ( )
import           Data.Type.BitRecords.Core
import           Data.Proxy

bitBuffer64Printer
  :: HasFunctionBuilder BitBuilder a => a -> ToFunction BitBuilder a String
bitBuffer64Printer =
  toFunction . mapAccumulator (printByteString . writeBits) . toFunctionBuilder

-- | Print a 'SB.Builder' to a space seperated series of hexa-decimal bytes.
printByteString :: B.ByteString -> String
printByteString b =
  ("<< " ++) $ (++ " >>") $ unwords $ printf "%0.2x" <$> B.unpack b



-- | Print a 'SB.Builder' to a space seperated series of hexa-decimal bytes.
printBuilder :: SB.Builder -> String
printBuilder b =
  ("<< " ++) $ (++ " >>") $ unwords $ printf "%0.2x" <$> B.unpack
    (SB.toLazyByteString b)


instance (KnownNat n, n <= 64) => Arbitrary (B n) where
  arbitrary = do
    let h  = 2 ^ (n' - 1) - 1
        n' = fromIntegral (natVal (Proxy @n)) :: Int
    x <- choose (0, h + h + 1)
    return (B x)
