{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints  #-}
module Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor where

import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.Util.FullBox
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.SyncLayerConfigDescriptor

-- * Esd Box

type EsdBox = Box (FullBox Esd 0)
newtype Esd = Esd BuilderWithSize deriving (IsBoxContent)
instance IsBox Esd

type instance BoxTypeSymbol Esd = "esds"

esdBox :: forall (record :: Extends (Descriptor 'ES_Descr)) (rendered :: BitRecord) .
         ( HasFunctionBuilder BitBuilder (Proxy rendered)
         , rendered ~ (RenderEsDescr record))
       => Proxy record -> ToFunction BitBuilder (Proxy rendered) EsdBox
esdBox =
  toFunction
  . esdBoxHoley

esdBoxHoley :: forall (record :: Extends (Descriptor 'ES_Descr)) r (rendered :: BitRecord) .
               ( HasFunctionBuilder BitBuilder (Proxy rendered)
               , rendered ~ (RenderEsDescr record)
               )
             => Proxy record -> FunctionBuilder EsdBox r (ToFunction BitBuilder (Proxy rendered) r)
esdBoxHoley _p =
  mapAccumulator (fullBox 0 . Esd) $
  builderBoxConstructor (Proxy @rendered)

type RenderEsDescr (d :: Extends (Descriptor 'ES_Descr)) =
  BitRecordOfDescriptor $ (From d)

-- * Esd Record

data ESDescriptor
  :: Extends (FieldValue "esId" Nat)
  -> Maybe (Extends (FieldValue "depEsId" Nat))
    -- TODO Improve the custom field and also the sizedstring API
  -> Maybe (Extends (BitRecordField ('MkFieldCustom :: BitField ASizedString ASizedString (urlSize :: Nat))))
  -> Maybe (Extends (FieldValue "ocrEsId" Nat))
  -> Extends (FieldValue "streamPrio" Nat)
  -> Extends (Descriptor 'DecoderConfigDescr)
  -> Extends (Descriptor 'SLConfigDescr)
  -> Extends (Descriptor 'ES_Descr)

-- | ISO-14496-14 section 3.1.2 defines restrictions of the elementary stream
-- descriptor.
-- TODO seperate this and other modules so theres the same seperation as in between
-- the parts of the standard.
type ESDescriptorMp4File esId decConfigDescr =
  ESDescriptor esId 'Nothing 'Nothing
               'Nothing DefaultStreamPrio
               decConfigDescr Mp4SyncLayerDescriptor

type DefaultEsId = StaticFieldValue "esId" 1
type DefaultStreamPrio = StaticFieldValue "streamPrio" 0

type instance
  From (ESDescriptor esId depEsId url ocrEsId streamPrio decConfig slConfig) =
  'MkDescriptor
     ("esId" @: FieldU16 :~ esId
      .+: "depEsIdFlag" @: FlagJust depEsId
      .+: "urlFlag" @: FlagJust url
      .+: "ocrEsIdFlag" @: FlagJust ocrEsId
      .+: "streamPriority" @: Field 5 :~ streamPrio
      .+: ("depEsId" @: FieldU16 :+? depEsId)
      :+: (From (OptionalRecordOf (Fun1 RecordField) url))
      :+: ("ocrEsId" @: FieldU16 :+? ocrEsId)
      :+: (BitRecordOfDescriptor $ From decConfig)
      :+: (BitRecordOfDescriptor $ From slConfig)

      -- TODO add the rest of the ESDescriptor
     )
