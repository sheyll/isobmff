{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.DecoderConfigDescriptor where

import           Data.ByteString.Mp4.Boxes.BaseDescriptor
import           Data.ByteString.Mp4.Boxes.DecoderSpecificInfo
import           Data.Type.BitRecords
import           Data.Kind.Extra
import           GHC.TypeLits

-- | Information about what decoder is required for the an elementary stream.
-- The stream type indicates the general category of the stream and.
data DecoderConfigDescriptor
       (ot :: ObjectTypeIndication)
       (st :: StreamType)
          :: [To (DecoderSpecificInfo ot st)]
          -> [To (Descriptor 'ProfileLevelIndicationIndexDescr)]
          -> To (Descriptor 'DecoderConfigDescr)

type instance From (DecoderConfigDescriptor ot st di ps) =
  'MkDescriptor (DecoderConfigDescriptorBody ot st di ps)

type family
    DecoderConfigDescriptorBody
      ot st
      (di :: [To (DecoderSpecificInfo ot st)])
      (ps :: [To (Descriptor 'ProfileLevelIndicationIndexDescr)])
        :: BitRecord
  where
    DecoderConfigDescriptorBody ot st di ps =
      BitRecordOfEnum (SetEnum "objectTypeIndication" ObjectTypeIndicationEnum ot)
              :+: BitRecordOfEnum (SetEnum "objectTypeIndication" StreamTypeEnum st)
              :+: "upstream" @: Flag
              .+: "reserved" @: Field 1        :=  1
              .+: "bufferSizeDB" @: Field 24
              .+: From ("maxBitrate" @:: Konst ('RecordField FieldU32))
              :+: From ("avgBitrate" @:: Konst ('RecordField FieldU32))
              :+: From (BitRecordOfList
                        (DescriptorOfDecoderSpecificInfo
                         :^>>>: BitRecordOfDescriptor)
                        (di ?:: LengthIn 0 1))
              :+: From (BitRecordOfList
                        (Extract :>>>: BitRecordOfDescriptor)
                        (ps ?:: LengthIn 0 255))

-- ** 'ProfileLevelIndicationIndexDescriptor'

data ProfileLevelIndicationIndexDescriptor
  :: To (FieldValue "profileLevelIndicationIndex" Nat)
  -> To (Descriptor 'ProfileLevelIndicationIndexDescr)

type instance From (ProfileLevelIndicationIndexDescriptor val) =
  'MkDescriptor
  ('BitRecordMember (FieldU8 :~ val))
