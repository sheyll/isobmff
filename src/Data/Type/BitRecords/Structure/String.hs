{-# LANGUAGE UndecidableInstances #-}

module Data.Type.BitRecords.Structure.String
  ( FixSizeStringStructure
  , utf8Bits
  )
where

import qualified Language.Haskell.TH           as TH
import qualified Language.Haskell.TH.Quote     as TH
import           GHC.TypeLits
import           Data.Type.Pretty
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Data.Proxy
import           Data.Kind.Extra
import           Data.Type.BitRecords.Structure

-- TODO Refactor

-- * String Fields

-- | A type level symbol paired with a type level length, that determines how
-- many bits the representation will use.
-- The resulting structure has __exactly__ that size.
-- Note: That currently multibyte character support is missing.
data FixSizeStringStructure :: Nat -> Extends (Structure 'FixSize)

type instance GetStructureSize (FixSizeStringStructure size) = size

type instance PrettyStructure (FixSizeStringStructure size) =
     "FixSizeStringStructure"  <:> PutNat size <++> PutStr "bits"


-- | Create a 'FixSizeStringStructure' from a utf-8 string
utf8Bits :: TH.QuasiQuoter
utf8Bits = TH.QuasiQuoter undefined undefined mkSizedStr undefined
 where
  mkSizedStr :: String -> TH.Q TH.Type
  mkSizedStr str = do
    let strT       = TH.LitT (TH.StrTyLit str)
        byteCount  = fromIntegral (B.length (E.encodeUtf8 (T.pack str)))
        byteCountT = TH.LitT (TH.NumTyLit byteCount)
    return $ TH.PromotedT ''FixSizeStringStructure `TH.AppT` strT `TH.AppT` byteCountT


-- instance
--   forall (size :: Nat)
--     (str :: Symbol)
--     (bytes :: Nat)
--     (f :: Extends (BitRecordField ('MkFieldCustom :: BitField ASizedString ASizedString size))) .
--       (KnownSymbol str)
--     => HasFunctionBuilder BitBuilder (Proxy (f := 'MkASizedString str bytes))
--   where
--   toFunctionBuilder _ = immediate
--     (appendStrictByteString (E.encodeUtf8 (T.pack (symbolVal (Proxy @str)))))

-- instance
--   forall (size :: Nat)
--     (str :: Symbol)
--     (bytes :: Nat)
--     (f :: Extends (BitField ASizedString ASizedString size)) .
--       (KnownSymbol str)
--     => HasFunctionBuilder BitBuilder (Proxy (f :=. 'MkASizedString str bytes))
--   where
--   toFunctionBuilder _ = immediate
--     (appendStrictByteString (E.encodeUtf8 (T.pack (symbolVal (Proxy @str)))))
